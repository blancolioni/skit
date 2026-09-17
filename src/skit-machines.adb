with Ada.Calendar;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Skit.Debug;

package body Skit.Machines is

   --  See the spec: silence the "Pre not enforced on inlined subprogram"
   --  warning for the inlined subprograms declared in this body too.
   pragma Warnings
     (Off, "aspect ""Pre"" not enforced on inlined subprogram*");

   Trace      : constant Boolean := False;
   Instrument : constant Boolean := False;

   function Apply
     (This        : in out Instance'Class;
      Left, Right : Object)
      return Object;

   function Pop
     (This : in out Instance'Class;
      From : Internal_Register)
      return Object
     with Inline_Always;

   procedure Push
     (This  : in out Instance'Class;
      To    : Internal_Register;
      Value : Object)
     with Inline_Always;

   procedure Evaluate_Application
     (This      : in out Instance'Class;
      User_Data : access User_Data_Interface'Class);

   procedure GC
     (This : in out Instance'Class;
      Xs   : in out Object_Array);

   function Mark_Foreign_Object
     (This : in out Instance'Class;
      O    : Object)
      return Boolean;
   procedure Mark_Pinned_Foreign (This : in out Instance'Class);
   procedure Mark_Foreign_Roots
     (This : in out Instance'Class;
      Xs   : Object_Array);
   function Discover_Foreign (This : in out Instance'Class) return Boolean;
   procedure Sweep_Foreign (This : in out Instance'Class);

   procedure Free_Reference is
     new Ada.Unchecked_Deallocation
       (Foreign_Object_Interface'Class, Foreign_Reference);

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
      if Instrument then
         This.Alloc_Count := @ + 1;
      end if;
      if Skit.Memory.Is_Full (This.Core) then
         declare
            Xs : Object_Array := [Left, Right];
         begin
            This.GC (Xs);

            if Skit.Memory.Is_Full (This.Core) then
               raise Storage_Error with "out of memory";
            end if;

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
      Key      : constant Object_Payload := Payload (Name);
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
      X     : constant Object := This.Pop;
   begin
      if Is_Application (X) then
         if Trace then
            Ada.Text_IO.Put_Line
              ("eval: " & This.Debug_Image (X));
         end if;
         This.Push (Control, X);
         This.Evaluate_Application (User_Data);
      else
         --  Integer, float, or primitive: already a value.
         This.Push (X);
      end if;
      This.Eval_Time := @ + (Clock - Start);
   end Evaluate;

   --------------------------
   -- Evaluate_Application --
   --------------------------

   procedure Evaluate_Application
     (This      : in out Instance'Class;
      User_Data : access User_Data_Interface'Class)
   is

      function Is_App (App : Object) return Boolean
      is (Is_Application (App));

      function Left (App : Object) return Object
      is (Skit.Memory.Left (This.Core, App));

      function Right (App : Object) return Object
      is (Skit.Memory.Right (This.Core, App));

      function Pop (Args : out Object_Array) return Boolean;

      function Top return Object
      is (Skit.Memory.Left (This.Core, This.Internal (Control)));

      function Is_Defined_Primitive
        (X : Object)
         return Boolean
      is (Is_Primitive_Function (X)
          and then Primitive_Function_Index (X) <= This.Prims.Last_Index);

      procedure Eval_Combinator (Combinator : Combinator_Payload);
      procedure Eval_Primitive (F : Object)
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
         while Is_App (This.Internal (Control)) loop
            if Trace then
               Ada.Text_IO.Put_Line
                 ("apply: "
                  & This.Debug_Image
                    (Right (Left (This.Internal (Control)))));
            end if;
            This.Push (Right (This.Pop (Control)));
            This.Apply;
         end loop;
         if Trace then
            Ada.Text_IO.Put_Line
              ("result: " & This.Debug_Image (Left (This.Internal (Stack))));
         end if;
      end Collect_Result;

      ---------------------
      -- Eval_Combinator --
      ---------------------

      procedure Eval_Combinator (Combinator : Combinator_Payload) is
         use Skit.Memory;
         Arg_Count : constant Natural :=
                       (case Combinator is
                           when Payload_I | Payload_Y             => 1,
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

         pragma Assert
           (not This.Advancing_Primitive,
            "combinator evaluated while Advance_Primitive is active");

         if Pop (X) then
            Changed := True;
            if Combinator = Payload_Y then
               --  Fixpoint by knot-tying.  The redex root X (1) is the node
               --  App (Y, f).  Rewrite it in place to App (f, X (1)) -- a
               --  self-referential cell whose own value is the fixpoint.
               --  Every recursive reference (the argument handed to f) is
               --  this one shared node, so when App (f, X (1)) reduces its
               --  result overwrites X (1) and later unfoldings reuse it:
               --  O(1) work per step and O(1) live space, versus the
               --  quadratic re-reduction of the combinator Y = S S I ....
               declare
                  F : constant Object := Right (X (1));
               begin
                  Set_Left  (This.Core, X (1), F);
                  Set_Right (This.Core, X (1), X (1));
                  This.Push (Control, X (1));
               end;
               return;
            end if;
            case Combinator is
               when Payload_Y =>
                  null;  --  handled above, before this case

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
            if Is_Application (It)
              and then Combinator not in Payload_I | Payload_K
            then
               --  S, B, C, S', B*, C' build a fresh top node unique to this
               --  redex, so overwrite the root with its contents directly
               --  rather than an App (I, It) indirection.  This avoids the
               --  identity-indirection chains that otherwise accumulate one
               --  cell per reduction and leak O(n) space.  I and K return an
               --  existing (possibly shared) argument, so they must keep the
               --  indirection to preserve sharing under later updates.
               Set_Left (This.Core, X (Arg_Count), Left (It));
               Set_Right (This.Core, X (Arg_Count), Right (It));
               This.Push (Control, X (Arg_Count));
            else
               Set_Left (This.Core, X (Arg_Count), Skit.I);
               Set_Right (This.Core, X (Arg_Count), It);
               This.Push (Control, It);
            end if;
         end if;

      end Eval_Combinator;

      --------------------
      -- Eval_Primitive --
      --------------------

      procedure Eval_Primitive (F : Object) is
         P  : constant Natural := Primitive_Function_Index (F);
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
            This.Push (F);

            for I in 1 .. Fn.Argument_Count loop
               This.Push (This.Pop (Control));
               This.Apply;
            end loop;

            --  Park the redex root and the tagged pending call on the
            --  secondary stack.  Both stay reachable from the stack (q_N)
            --  until parked, so a collection during Apply cannot free them.
            declare
               Pending : constant Object :=
                           Skit.Memory.Left (This.Core, This.Internal (Stack));
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
         Frame : Object renames This.R (1);
         Partial : Object renames This.R (2);
      begin
         This.Advancing_Primitive := True;
         Frame := Skit.Memory.Left
                     (This.Core, This.Internal (Secondary_Stack));
         --  Frame = App (Partial, Suspension); it remains on the secondary
         --  stack (a GC root) for the whole traversal, so every argument and
         --  the redex root stay reachable through it.

         loop
            Partial := Left (Frame);
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
                              Primitive_Function_Index (Walk);
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
                        This.Advancing_Primitive := False;
                        return;
                  end case;
               end;
            end;
         end loop;

         --  Every argument is now on the stack; invoke the primitive.
         declare
            Prim    : constant Object := Left (Frame);
            P_Index : constant Natural :=
                        Primitive_Function_Index (Prim);
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
         This.Advancing_Primitive := False;

      end Advance_Primitive;

      ---------------------
      -- Eval_Suspension --
      ---------------------

      procedure Eval_Suspension is
         Top : constant Object :=
                 (if Is_App (This.Internal (Secondary_Stack))
                  then Left (This.Internal (Secondary_Stack))
                  else Nil);
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
         P : Object := This.Internal (Control);
      begin
         for Arg of Args loop
            if P = Nil then
               return False;
            end if;
            Arg := Skit.Memory.Left (This.Core, P);
            P := Skit.Memory.Right (This.Core, P);
         end loop;
         This.Internal (Control) := P;
         return True;
      end Pop;

   begin

      while Changed loop
         Changed := False;
         It := This.Pop (Control);

         while Is_Application (It) loop
            if Trace then
               Ada.Text_IO.Put_Line
                 ("push: " & This.Debug_Image (Right (It)));
            end if;
            This.Push (Control, It);
            It := Skit.Memory.Left (This.Core, Top);
         end loop;

         if Trace then
            Ada.Text_IO.Put_Line
              ("stop: " & This.Debug_Image (It));
         end if;

         if Is_Combinator (It) then
            Eval_Combinator (Payload (It));
         elsif Is_Primitive_Function (It) then
            if Is_Defined_Primitive (It) then
               Eval_Primitive (It);
            else
               raise Constraint_Error with
                 "undefined primitive: " & This.Debug_Image (It);
            end if;
         elsif Is_Primitive (It) then
            raise Constraint_Error with
              "invalid primitive:" & Payload (It)'Image;
         else
            This.Push (It);
         end if;

         if not Changed then

            --  A non-primitive head (a scalar result) was already pushed onto
            --  the stack by the else branch above, where it also feeds the
            --  strict-argument resumption in Eval_Suspension below.  Only a
            --  primitive head (a bare combinator or an under-saturated partial
            --  application) still needs pushing here; pushing an atom again
            --  would leave a duplicate on the stack.
            --
            --  This must happen before Eval_Suspension, not once after this
            --  loop has fully exited: an under-saturated combinator can be
            --  exactly the value a pending primitive call's strict argument
            --  just reduced to (e.g. unit's Scott encoding collapsing to the
            --  bare I combinator).  When it is, Eval_Suspension immediately
            --  resumes Advance_Primitive, whose Call_Primitive pops exactly
            --  Argument_Count items off this stack expecting to find that
            --  value among them.  Deferring the push left it stranded in It,
            --  one item short, and Call_Primitive's Pop read past the real
            --  arguments into unrelated stack contents.

            if Is_Primitive (It) then
               This.Push (It);
            end if;

            Eval_Suspension;
         end if;
      end loop;

      Collect_Result;

   end Evaluate_Application;

   --------
   -- GC --
   --------

   procedure GC
     (This : in out Instance'Class;
      Xs   : in out Object_Array)
   is
      use Skit.Memory;
   begin
      if Trace or else Instrument then
         declare
            use Ada.Calendar;
            Start : constant Time := Clock;
         begin
            Ada.Text_IO.Put_Line ("GC");
            Before_GC (This.Core);
            for X of This.Internal loop
               Mark (This.Core, X);
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
            This.Mark_Pinned_Foreign;
            This.Mark_Foreign_Roots (Xs);

            loop
               GC (This.Core);
               exit when not This.Discover_Foreign;
            end loop;

            After_GC (This.Core);
            This.Sweep_Foreign;
            This.GC_Time := @ + (Clock - Start);
         end;
      else
         Before_GC (This.Core);
         for X of This.Internal loop
            Mark (This.Core, X);
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
         This.Mark_Pinned_Foreign;
         This.Mark_Foreign_Roots (Xs);

         loop
            GC (This.Core);
            exit when not This.Discover_Foreign;
         end loop;

         After_GC (This.Core);
         This.Sweep_Foreign;
      end if;

      This.GC_Count := @ + 1;
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
      Position : constant Cursor := This.Environment.Find (Payload (Name));
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

   function Pop
     (This : in out Instance'Class)
      return Object
   is
      S : Object renames This.Internal (Stack);
   begin
      return X : constant Object := Skit.Memory.Left (This.Core, S) do
         S := Skit.Memory.Right (This.Core, S);
      end return;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop
     (This : in out Instance'Class;
      From : Internal_Register)
      return Object
   is
      S : Object renames This.Internal (From);
   begin
      return X : constant Object := Skit.Memory.Left (This.Core, S) do
         S := Skit.Memory.Right (This.Core, S);
      end return;
   end Pop;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left
     (This : in out Instance'Class;
      App  : Object;
      To   : Object)
   is
   begin
      Skit.Memory.Set_Left (This.Core, App, To);
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right
     (This : in out Instance'Class;
      App  : Object;
      To   : Object)
   is
   begin
      Skit.Memory.Set_Right (This.Core, App, To);
   end Set_Right;

   -----------------
   -- Stack_Empty --
   -----------------

   function Stack_Empty
     (This : Instance'Class)
      return Boolean
   is (This.Internal (Stack) = Nil);

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
      return Primitive_Function (This.Prims.Last_Index);
   end Primitive;

   ----------
   -- Push --
   ----------

   procedure Push
     (This  : in out Instance'Class;
      Value : Object)
   is
      S : Object renames This.Internal (Stack);
   begin
      S := This.Apply (Value, S);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This  : in out Instance'Class;
      To    : Internal_Register;
      Value : Object)
   is
      S : Object renames This.Internal (To);
   begin
      S := This.Apply (Value, S);
   end Push;

   ---------------------------
   -- Register_Object_Class --
   ---------------------------

   procedure Register_Object_Class
     (This        : in out Instance'Class;
      Name        : String;
      Deserialize : Deserializer)
   is
   begin
      This.Classes.Include (Name, Deserialize);
   end Register_Object_Class;

   -----------------
   -- Bind_Object --
   -----------------

   function Bind_Object
     (This : in out Instance'Class;
      Obj  : not null Foreign_Reference)
      return Object
   is
      New_Slot : constant Foreign_Slot :=
                   (Ref => Obj, Pinned => True, Marked => False);
      Index    : Natural;
   begin
      if This.Free_Slots.Is_Empty then
         Index := Natural (This.Foreign.Length);
         This.Foreign.Append (New_Slot);
      else
         Index := This.Free_Slots.Last_Element;
         This.Free_Slots.Delete_Last;
         This.Foreign.Replace_Element (Index, New_Slot);
      end if;
      return Foreign_Object (Index);
   end Bind_Object;

   -----------
   -- Unpin --
   -----------

   procedure Unpin
     (This : in out Instance'Class;
      O    : Object)
   is
      Index : constant Natural := Foreign_Object_Index (O);
      Slot  : Foreign_Slot     := This.Foreign (Index);
   begin
      Slot.Pinned := False;
      This.Foreign.Replace_Element (Index, Slot);
   end Unpin;

   -------------------------
   -- Mark_Foreign_Object --
   -------------------------

   --  If O is a foreign object that is not yet marked this cycle, mark it and
   --  forward its Object children into to-space (via Visit).  Returns True iff
   --  it was newly marked.  The mark flag makes this idempotent, which is what
   --  breaks cycles: a foreign object reached a second time is skipped.

   function Mark_Foreign_Object
     (This : in out Instance'Class;
      O    : Object)
      return Boolean
   is
      procedure Forward (Child : in out Object);

      procedure Forward (Child : in out Object) is
      begin
         Skit.Memory.Mark (This.Core, Child);
      end Forward;

      Result : Boolean := False;
   begin
      if Is_Foreign_Object (O) then
         declare
            Index : constant Natural := Foreign_Object_Index (O);
         begin
            if Index < Natural (This.Foreign.Length) then
               declare
                  Slot : Foreign_Slot := This.Foreign (Index);
               begin
                  if Slot.Ref /= null and then not Slot.Marked then
                     Slot.Marked := True;
                     This.Foreign.Replace_Element (Index, Slot);
                     Slot.Ref.Visit (Forward'Access);
                     Result := True;
                  end if;
               end;
            end if;
         end;
      end if;
      return Result;
   end Mark_Foreign_Object;

   -------------------------
   -- Mark_Pinned_Foreign --
   -------------------------

   --  Mark every pinned foreign object (an unconditional root) and forward its
   --  children.  Called once during the mark phase, before the Cheney scan.

   procedure Mark_Pinned_Foreign (This : in out Instance'Class) is
   begin
      for K in 1 .. Natural (This.Foreign.Length) loop
         declare
            Index   : constant Natural := K - 1;
            Discard : Boolean;
         begin
            if This.Foreign (Index).Pinned then
               Discard := This.Mark_Foreign_Object (Foreign_Object (Index));
               pragma Unreferenced (Discard);
            end if;
         end;
      end loop;
   end Mark_Pinned_Foreign;

   ------------------------
   -- Mark_Foreign_Roots --
   ------------------------

   --  Mark foreign objects referenced *directly* by a root (a register, an
   --  environment value, or a cell being appended) rather than through a live
   --  cell -- Discover_Foreign only scans cells, so these would otherwise be
   --  missed and wrongly swept.

   procedure Mark_Foreign_Roots
     (This : in out Instance'Class;
      Xs   : Object_Array)
   is
      procedure Mark_One (O : Object);

      procedure Mark_One (O : Object) is
         Discard : constant Boolean := This.Mark_Foreign_Object (O);
      begin
         pragma Unreferenced (Discard);
      end Mark_One;
   begin
      for X of This.Internal loop
         Mark_One (X);
      end loop;
      for X of This.R loop
         Mark_One (X);
      end loop;
      for X of This.Environment loop
         Mark_One (X);
      end loop;
      for X of Xs loop
         Mark_One (X);
      end loop;
   end Mark_Foreign_Roots;

   ----------------------
   -- Discover_Foreign --
   ----------------------

   --  Walk the live cell set; for each foreign payload not yet marked, mark it
   --  and forward its children.  Returns True if any new object was marked, so
   --  the caller re-runs the Cheney scan (draining the newly forwarded
   --  children) and calls again -- reaching a fixpoint that marks every
   --  foreign object reachable through the heap, including nested and
   --  mutually-referencing ones.  Forwarding grows the live set past the
   --  snapshot Count; those cells are covered on the next round, after the
   --  scan drains them.

   function Discover_Foreign (This : in out Instance'Class) return Boolean is
      Progress : Boolean := False;
      Count    : constant Natural :=
                   Skit.Memory.Live_Cell_Count (This.Core);
      Left     : Object;
      Right    : Object;
   begin
      if This.Foreign.Is_Empty then
         return False;   --  no foreign objects: skip the live-cell walk
      end if;
      for K in 1 .. Count loop
         Skit.Memory.Live_Cell (This.Core, K - 1, Left, Right);
         if This.Mark_Foreign_Object (Left) then
            Progress := True;
         end if;
         if This.Mark_Foreign_Object (Right) then
            Progress := True;
         end if;
      end loop;
      return Progress;
   end Discover_Foreign;

   -------------------
   -- Sweep_Foreign --
   -------------------

   --  After discovery has marked every reachable foreign object, free the
   --  rest (dispatching Free + reclaim the slot for reuse) and clear the mark
   --  on the survivors for the next collection.

   procedure Sweep_Foreign (This : in out Instance'Class) is
   begin
      for K in 1 .. Natural (This.Foreign.Length) loop
         declare
            Index : constant Natural := K - 1;
            Slot  : Foreign_Slot     := This.Foreign (Index);
         begin
            if Slot.Ref /= null then
               if Slot.Marked then
                  Slot.Marked := False;
                  This.Foreign.Replace_Element (Index, Slot);
               else
                  Slot.Ref.Free;
                  Free_Reference (Slot.Ref);
                  This.Foreign.Replace_Element
                    (Index, (Ref => null, Pinned => False, Marked => False));
                  This.Free_Slots.Append (Index);
               end if;
            end if;
         end;
      end loop;
   end Sweep_Foreign;

   -------------------------
   -- Foreign_Object_Ref --
   -------------------------

   function Foreign_Object_Ref
     (This : Instance'Class;
      O    : Object)
      return Foreign_Reference
   is (This.Foreign (Foreign_Object_Index (O)).Ref);

   -------------------------
   -- Deserialize_Foreign --
   -------------------------

   function Deserialize_Foreign
     (This     : Instance'Class;
      Class    : String;
      Bytes    : Ada.Streams.Stream_Element_Array;
      Children : Object_Array)
      return Foreign_Reference
   is
      Pos : constant Class_Maps.Cursor := This.Classes.Find (Class);
   begin
      if Class_Maps.Has_Element (Pos) then
         return Class_Maps.Element (Pos) (Bytes, Children);
      else
         return null;
      end if;
   end Deserialize_Foreign;

   --------------------------
   -- Free_Foreign_Objects --
   --------------------------

   procedure Free_Foreign_Objects (This : in out Instance'Class) is
   begin
      for K in 1 .. Natural (This.Foreign.Length) loop
         declare
            Index : constant Natural := K - 1;
            Slot  : Foreign_Slot     := This.Foreign (Index);
         begin
            if Slot.Ref /= null then
               Slot.Ref.Free;
               Free_Reference (Slot.Ref);
               This.Foreign.Replace_Element
                 (Index, (Ref => null, Pinned => False, Marked => False));
            end if;
         end;
      end loop;
      This.Foreign.Clear;
      This.Free_Slots.Clear;
   end Free_Foreign_Objects;

end Skit.Machines;
