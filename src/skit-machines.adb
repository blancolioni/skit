with Ada.Calendar;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Skit.Debug;
with Skit.Memory.Report;

package body Skit.Machines is

   Trace : constant Boolean := False;

   function Apply
     (This        : in out Instance'Class;
      Left, Right : Object)
      return Object;

   function Pop
     (This : in out Instance'Class;
      From : Internal_Stack)
      return Object
     with Inline_Always;

   procedure Push
     (This  : in out Instance'Class;
      To    : Internal_Stack;
      Value : Object)
     with Inline_Always;

   procedure Evaluate_Application
     (This      : in out Instance'Class;
      User_Data : access User_Data_Interface'Class);

   procedure GC
     (This : in out Instance'Class;
      Xs   : in out Object_Array);

   ------------
   -- Append --
   ------------

   function Append
     (This        : in out Instance'Class;
      Left, Right : Object)
      return Object
   is
   begin
      if Skit.Memory.Is_Full (This.Core) then
         raise Constraint_Error with
           "machine memory overflow in Append";
      end if;
      return Skit.Memory.Append (This.Core, Left, Right);
   end Append;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (This : in out Instance'Class)
   is
      Right : constant Object := This.Pop;
      Left  : constant Object := This.Pop;
   begin
      This.Push (This.Apply (Left, Right));
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (This        : in out Instance'Class;
      Left, Right : Object)
      return Object
   is
   begin
      if Skit.Memory.Is_Full (This.Core) then
         declare
            Xs : Object_Array := [Left, Right];
         begin
            This.GC (Xs);

            if Skit.Memory.Is_Full (This.Core) then
               raise Storage_Error with "out of memory";
            end if;

            Skit.Memory.Report (This.Core);

            return Skit.Memory.Append (This.Core, Xs (1), Xs (2));
         end;
      else
         return Skit.Memory.Append (This.Core, Left, Right);
      end if;
   end Apply;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This  : in out Instance'Class;
      Name  : Object;
      Value : Object)
   is
      Key      : constant Object_Payload := Name.Payload;
      Position : constant Environment_Maps.Cursor :=
                   This.Environment.Find (Key);
   begin
      if Environment_Maps.Has_Element (Position) then
         This.Environment.Replace_Element (Position, Value);
      else
         This.Environment.Insert (Key, Value);
      end if;
   end Bind;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image
     (This : Instance'Class;
      X    : Object)
      return String
   is
   begin
      return Skit.Debug.Image (X, This.Core);
   end Debug_Image;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (This      : in out Instance'Class;
      User_Data : access User_Data_Interface'Class)
   is
      use Ada.Calendar;
      Start : constant Time := Clock;
      X : constant Object := This.Pop;
   begin
      case X.Tag is
         when Integer_Object =>
            This.Push (X);
         when Float_Object =>
            This.Push (X);
         when Primitive_Object =>
            This.Push (X);
         when Application_Object =>
            if Trace then
               Ada.Text_IO.Put_Line
                 ("eval: " & This.Debug_Image (X));
            end if;
            This.Push (Control, X);
            This.Evaluate_Application (User_Data);
      end case;
      This.Eval_Time := @ + (Clock - Start);
   end Evaluate;

   --------------------------
   -- Evaluate_Application --
   --------------------------

   procedure Evaluate_Application
     (This  : in out Instance'Class;
      User_Data : access User_Data_Interface'Class)
   is

      function Is_App (App : Object) return Boolean
      is (App.Tag = Application_Object);

      function Left (App : Object) return Object
      is (Skit.Memory.Left (This.Core, App));

      function Right (App : Object) return Object
      is (Skit.Memory.Right (This.Core, App));

      function Pop (Args : out Object_Array) return Boolean;

      function Top return Object
      is (Skit.Memory.Left (This.Core, Top (This.Stacks (Control))));

      function Is_Defined_Primitive
        (Payload : Object_Payload)
         return Boolean
      is (Payload in Primitive_Function_Payload
          and then Natural (Payload - Primitive_Function_Payload'First)
          <= This.Prims.Last_Index);

      procedure Eval_Combinator (Combinator : Combinator_Payload);
      procedure Eval_Primitive (F : Primitive_Function_Payload)
        with Pre => Is_Defined_Primitive (F);

      procedure Eval_Suspension;

      procedure Advance_Primitive;

      procedure Call_Primitive
        (Evaluator      : Primitive_Evaluator_Interface'Class);

      procedure Collect_Result;

      Changed : Boolean := True;
      It      : Object  := Nil;

      --------------------
      -- Call_Primitive --
      --------------------

      procedure Call_Primitive
        (Evaluator      : Primitive_Evaluator_Interface'Class)
      is
         Arguments : Object_Array (1 .. Evaluator.Argument_Count);
      begin
         for Arg of Arguments loop
            Arg := This.Pop;
         end loop;
         if Trace then
            Ada.Text_IO.Put ("calling with args:");
            for Arg of Arguments loop
               Ada.Text_IO.Put ("," & This.Debug_Image (Arg));
            end loop;
            Ada.Text_IO.New_Line;
         end if;
         This.Push (Evaluator.Evaluate (User_Data, Arguments));
      end Call_Primitive;

      --------------------
      -- Collect_Result --
      --------------------

      procedure Collect_Result is
      begin
         if Trace then
            Ada.Text_IO.Put_Line ("collecting result");
         end if;
         while not Is_Empty (This.Stacks (Control)) loop
            if Trace then
               Ada.Text_IO.Put_Line
                 ("apply: "
                  & This.Debug_Image
                    (Right (Top (This.Stacks (Control)))));
            end if;
            This.Push (Right (This.Pop (Control)));
            This.Apply;
         end loop;
         if Trace then
            Ada.Text_IO.Put_Line
              ("result: " & This.Debug_Image (Top (This.Stacks (Stack))));
         end if;
      end Collect_Result;

      ---------------------
      -- Eval_Combinator --
      ---------------------

      procedure Eval_Combinator (Combinator : Combinator_Payload) is
         use Skit.Memory;
         Arg_Count : constant Natural :=
                       (case Combinator is
                           when Payload_I                         => 1,
                           when Payload_K                         => 2,
                           when Payload_S | Payload_C | Payload_B => 3,
                           when Payload_C_Prime | Payload_B_Star  => 4,
                           when Payload_S_Prime                   => 4);

         X : Object_Array renames This.R (1 .. Arg_Count);

         procedure Apply with Inline_Always;
         procedure Push (Index : Positive)
           with Inline_Always, Pre => Index in 1 .. Arg_Count;

         -----------
         -- Apply --
         -----------

         procedure Apply is
         begin
            This.Apply;
         end Apply;

         ----------
         -- Push --
         ----------

         procedure Push (Index : Positive) is
         begin
            This.Push (Right (This.Core, X (Index)));
         end Push;

      begin

         if Pop (X) then
            Changed := True;
            case Combinator is
               when Payload_I =>
                  Push (1);

               when Payload_K =>
                  Push (1);

               when Payload_S =>
                  Push (1);
                  Push (3);
                  Apply;
                  Push (2);
                  Push (3);
                  Apply;
                  Apply;

               when Payload_B =>
                  Push (1);
                  Push (2);
                  Push (3);
                  Apply;
                  Apply;

               when Payload_C =>
                  Push (1);
                  Push (3);
                  Apply;
                  Push (2);
                  Apply;

               when Payload_S_Prime =>
                  Push (1);
                  Push (2);
                  Push (4);
                  Apply;
                  Apply;
                  Push (3);
                  Push (4);
                  Apply;
                  Apply;

               when Payload_B_Star =>
                  Push (1);
                  Push (2);
                  Push (3);
                  Push (4);
                  Apply;
                  Apply;
                  Apply;

               when Payload_C_Prime =>
                  Push (1);
                  Push (2);
                  Push (4);
                  Apply;
                  Apply;
                  Push (3);
                  Apply;
            end case;

            It := This.Pop;
            Set_Left (This.Core, X (Arg_Count), Skit.I);
            Set_Right (This.Core, X (Arg_Count), It);
            This.Push (Control, It);
         end if;

      end Eval_Combinator;

      --------------------
      -- Eval_Primitive --
      --------------------

      procedure Eval_Primitive (F : Primitive_Function_Payload) is
         P  : constant Natural :=
                Natural
                  (F - Primitive_Function_Payload'First);
         Fn : Primitive_Evaluator_Interface'Class renames This.Prims (P);
      begin
         if Fn.Argument_Count = 0 then
            Call_Primitive (Fn);
            Changed := True;
         else
            --  Build the pending call: the primitive applied to all of its
            --  argument spine nodes.  The result, on top of the stack, is a
            --  left-nested chain q_N whose Left spine bottoms out at the
            --  primitive and whose Right at each level is the original spine
            --  node carrying that argument.  Right (q_N) is the redex root.
            This.Push ((F, Primitive_Object));

            for I in 1 .. Fn.Argument_Count loop
               This.Push (This.Pop (Control));
               This.Apply;
            end loop;

            --  Park the redex root and the tagged pending call on the
            --  secondary stack.  Both stay reachable from the stack (q_N)
            --  until parked, so a collection during Apply cannot free them.
            declare
               Pending : constant Object := Top (This.Stacks (Stack));
            begin
               This.Push (Secondary_Stack, Right (Pending));
            end;

            This.Push (Skit.Suspension);
            This.Apply;
            This.Push (Secondary_Stack, This.Pop);

            Advance_Primitive;
         end if;

      end Eval_Primitive;

      -----------------------
      -- Advance_Primitive --
      -----------------------

      --  Drive a pending primitive call parked on the secondary stack as
      --  App (Partial, Suspension), with the redex root parked beneath it.
      --  Arguments are processed from the highest index down to 1.  A lazy
      --  argument is pushed onto the stack unevaluated; a strict argument is
      --  forced (pushed onto Control for the main loop to reduce, after which
      --  Eval_Suspension resumes here).  When every argument has reached the
      --  stack the evaluator is called and the redex root is overwritten with
      --  an indirection to the result.

      procedure Advance_Primitive is
         Frame : constant Object := Top (This.Stacks (Secondary_Stack));
         --  Frame = App (Partial, Suspension); it remains on the secondary
         --  stack (a GC root) for the whole traversal, so every argument and
         --  the redex root stay reachable through it.
      begin
         loop
            declare
               Partial : constant Object := Left (Frame);
            begin
               exit when not Is_App (Partial);   --  bare primitive: go call

               declare
                  Arg   : constant Object := Right (Right (Partial));  -- arg_j
                  Index : Natural := 0;
                  Walk  : Object  := Partial;
               begin
                  --  Position of this argument = number of App wrappers left.
                  while Is_App (Walk) loop
                     Index := Index + 1;
                     Walk  := Left (Walk);
                  end loop;

                  declare
                     F_Index : constant Natural :=
                                 Natural
                                   (Walk.Payload
                                    - Primitive_Function_Payload'First);
                     Fn      : Primitive_Evaluator_Interface'Class
                     renames This.Prims (F_Index);
                  begin
                     case Fn.Argument_Modes (Index) is
                        when Lazy =>
                           --  Lazy: pass the thunk unevaluated, then advance.
                           --  Push before mutating Frame so Arg stays
                           --  reachable through Frame across any collection
                           --  in Push.
                           This.Push (Arg);
                           Skit.Memory.Set_Left
                             (This.Core, Frame, Left (Partial));
                        when Strict =>
                           --  Strict: force the argument.  Push it onto
                           --  Control (a GC root) first, then advance Frame.
                           This.Push (Control, Arg);
                           Skit.Memory.Set_Left
                             (This.Core, Frame, Left (Partial));
                           Changed := True;
                           return;
                     end case;
                  end;
               end;
            end;
         end loop;

         --  Every argument is now on the stack; invoke the primitive.
         declare
            Prim    : constant Object := Left (Frame);
            P_Index : constant Natural :=
                        Natural (Prim.Payload
                                 - Primitive_Function_Payload'First);
            Fn      : Primitive_Evaluator_Interface'Class
            renames This.Prims (P_Index);
         begin
            It := This.Pop (Secondary_Stack);   --  drop the frame
            --  Leave the redex root parked on the secondary stack across the
            --  call so a collection inside Call_Primitive cannot free it.
            Call_Primitive (Fn);
            declare
               Result : constant Object := This.Pop;
               Root   : constant Object := This.Pop (Secondary_Stack);
            begin
               Skit.Memory.Set_Left (This.Core, Root, Skit.I);
               Skit.Memory.Set_Right (This.Core, Root, Result);
               This.Push (Control, Result);
            end;
         end;

         Changed := True;

      end Advance_Primitive;

      ---------------------
      -- Eval_Suspension --
      ---------------------

      procedure Eval_Suspension is
         Top : constant Object :=
                 (if Is_Empty (This.Stacks (Secondary_Stack))
                  then Nil
                  else Machines.Top (This.Stacks (Secondary_Stack)));
      begin
         if Is_App (Top) then
            if Right (Top) = Suspension then
               if Trace then
                  Ada.Text_IO.Put_Line
                    ("suspension: "
                     & This.Debug_Image (Left (Top)));
               end if;
               Advance_Primitive;
            end if;
         end if;
      end Eval_Suspension;

      ---------
      -- Pop --
      ---------

      function Pop (Args : out Object_Array) return Boolean is
         C : Stack_Type renames This.Stacks (Control);
      begin
         if C.Top < Args'Length then
            return False;
         end if;

         for Arg of Args loop
            Arg := Pop (C);
         end loop;
         return True;
      end Pop;

   begin

      while Changed loop
         Changed := False;
         It := This.Pop (Control);

         while It.Tag = Application_Object loop
            if Trace then
               Ada.Text_IO.Put_Line
                 ("push: " & This.Debug_Image (Right (It)));
            end if;
            This.Push (Control, It);
            It := Top;
         end loop;

         if Trace then
            Ada.Text_IO.Put_Line
              ("stop: " & This.Debug_Image (It));
         end if;

         if It.Tag = Primitive_Object then
            case It.Payload is
               when Combinator_Payload =>
                  Eval_Combinator (It.Payload);
               when Primitive_Function_Payload =>
                  if Is_Defined_Primitive (It.Payload) then
                     Eval_Primitive (It.Payload);
                  else
                     raise Constraint_Error with
                       "undefined primitive: " & This.Debug_Image (It);
                  end if;
               when others =>
                  raise Constraint_Error with
                    "invalid primitive:" & It.Payload'Image;
            end case;
         else
            This.Push (It);
         end if;

         if not Changed then
            Eval_Suspension;
         end if;
      end loop;

      --  A non-primitive head (a scalar result) was already pushed onto the
      --  stack by the else branch in the loop above, where it also feeds the
      --  strict-argument resumption in Eval_Suspension.  Only a primitive
      --  head (a bare combinator or an under-saturated partial application)
      --  still needs pushing here; pushing an atom again would leave a
      --  duplicate on the stack.
      if It.Tag = Primitive_Object then
         This.Push (It);
      end if;
      Collect_Result;

   end Evaluate_Application;

   --------
   -- GC --
   --------

   procedure GC
     (This : in out Instance'Class;
      Xs   : in out Object_Array)
   is
      use Ada.Calendar;
      use Skit.Memory;
      Start : constant Time := Clock;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("GC");
      end if;
      Before_GC (This.Core);
      for S of This.Stacks loop
         for X of S.Arr (1 .. S.Top) loop
            Mark (This.Core, X);
         end loop;
      end loop;
      for X of This.R loop
         Mark (This.Core, X);
      end loop;
      for X of This.Environment loop
         Mark (This.Core, X);
      end loop;
      for X of Xs loop
         Mark (This.Core, X);
      end loop;

      GC (This.Core);

      After_GC (This.Core);

      This.GC_Count := @ + 1;
      This.GC_Time := @ + (Clock - Start);
   end GC;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Instance'Class) is
   begin
      Skit.Memory.Initialize (This.Core);
   end Initialize;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (This : Instance'Class;
      Name : Object)
      return Object
   is
      use Environment_Maps;
      Position : constant Cursor := This.Environment.Find (Name.Payload);
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return Undefined;
      end if;
   end Lookup;

   ---------
   -- Pop --
   ---------

   function Pop (Stack : in out Stack_Type) return Object is
   begin
      return X : constant Object := Stack.Arr (Stack.Top) do
         Stack.Top := @ - 1;
      end return;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop
     (This : in out Instance'Class)
      return Object
   is
   begin
      return Pop (This.Stacks (Stack));
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop
     (This : in out Instance'Class;
      From : Internal_Stack)
      return Object
   is
   begin
      return Pop (This.Stacks (From));
   end Pop;

   -----------------
   -- Stack_Empty --
   -----------------

   function Stack_Empty
     (This : Instance'Class)
      return Boolean
   is (Is_Empty (This.Stacks (Stack)));

   ---------------
   -- Primitive --
   ---------------

   function Primitive
     (This      : in out Instance'Class;
      Primitive : Primitive_Evaluator_Interface'Class)
      return Object
   is
   begin
      This.Prims.Append (Primitive);
      return (Object_Payload (This.Prims.Last_Index)
              + Primitive_Function_Payload'First,
              Primitive_Object);
   end Primitive;

   procedure Push (Stack : in out Stack_Type;
                   Value : Object)
   is
   begin
      if Stack.Arr = null then
         Stack.Arr := new Object_Array (1 .. 4096);
      elsif Stack.Top = Stack.Arr'Last then
         declare
            procedure Free is
              new Ada.Unchecked_Deallocation
                (Object_Array, Object_Array_Access);
            Old_Stack : Object_Array_Access := Stack.Arr;
         begin
            Stack.Arr := new Object_Array (1 .. Old_Stack'Last * 2);
            Stack.Arr (Old_Stack'Range) := Old_Stack.all;
            Free (Old_Stack);
         end;
      end if;
      Stack.Top := @ + 1;
      Stack.Max := Natural'Max (Stack.Max, Stack.Top);
      Stack.Arr (Stack.Top) := Value;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This  : in out Instance'Class;
      Value : Object)
   is
   begin
      Push (This.Stacks (Stack), Value);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This  : in out Instance'Class;
      To    : Internal_Stack;
      Value : Object)
   is
   begin
      Push (This.Stacks (To), Value);
   end Push;

end Skit.Machines;
