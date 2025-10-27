with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Skit.Allocator;
with Skit.Debug;
with Skit.Impl.Memory;
with Skit.Containers;
with Skit.Primitives;

package body Skit.Impl.Machines is

   Trace : constant Boolean := False;

   package Primitive_Function_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Natural, Skit.Primitives.Abstraction'Class, Skit.Primitives."=");

   subtype Register is Positive range 1 .. 15;
   Max_Locals : constant := 16;

   type Temporary_Array is array (Skit.Machine.Temporary) of Object;

   type Internal_Register is (Stack, Control, Dump, Secondary_Stack);
   type Internal_Register_Array is array (Internal_Register) of Object;

   subtype Parent is Skit.Machine.Abstraction;

   type Instance is new Parent and Skit.Containers.Abstraction with
      record
         Core       : Skit.Allocator.Reference;
         Internal   : Internal_Register_Array := [others        => Nil];
         R          : Object_Array (Register);
         Locals     : Object_Array (1 .. Max_Locals) := [others => Nil];
         Temps      : Temporary_Array := [others => Nil];
         Prims      : Primitive_Function_Vectors.Vector;
         Reductions : Natural := 0;
         R_I        : Natural := 0;
         R_K        : Natural := 0;
         R_S        : Natural := 0;
         R_B        : Natural := 0;
         R_C        : Natural := 0;
         R_Sp       : Natural := 0;
         R_Bs       : Natural := 0;
         R_Cp       : Natural := 0;
      end record;
   type Reference is access all Instance'Class;

   overriding procedure Add_Container
     (This      : in out Instance;
      Container : not null access Skit.Containers.Abstraction'Class);

   overriding procedure Apply
     (This : in out Instance);

   overriding function Bind
     (This      : in out Instance;
      Primitive : Skit.Primitives.Abstraction'Class)
      return Object;

   overriding procedure Evaluate
     (This : in out Instance);

   overriding procedure Mark
     (This : in out Instance;
      Set  : not null access procedure (Marked_Object : in out Object));

   overriding function Left
     (This : Instance;
      App  : Object)
      return Object
   is (This.Core.Left (App));

   overriding function Right
     (This : Instance;
      App  : Object)
      return Object
   is (This.Core.Right (App));

   overriding procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object);

   overriding procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object);

   overriding function Pop
     (This   : in out Instance;
      Values : out Object_Array)
      return Boolean;

   overriding procedure Push
     (This   : in out Instance;
      Values : in out Object_Array);

   overriding function Top
     (This   : Instance)
      return Object;

   overriding procedure Set
     (This  : in out Instance;
      T     : Skit.Machine.Temporary;
      Value : Object);

   overriding function Get
     (This  : Instance;
      T     : Skit.Machine.Temporary)
      return Object
   is (This.Temps (T));

   overriding procedure Report
     (This : Instance);

   function Pop
     (This   : in out Instance'Class;
      Stack  : Internal_Register;
      Values : out Object_Array)
      return Boolean;

   function Top
     (This   : Instance'Class;
      Stack  : Internal_Register)
      return Object;

   function Pop
     (This   : in out Instance'Class;
      Stack  : Internal_Register)
      return Object;

   procedure Push
     (This  : in out Instance'Class;
      Stack : Internal_Register;
      Value : Object);

   procedure Push
     (This   : in out Instance'Class;
      Stack  : Internal_Register;
      Values : in out Object_Array);

   procedure Evaluate_Application
     (This  : in out Instance'Class);

   procedure Reset_State
     (This      : in out Instance'Class;
      Stack_Top : Object);

   procedure Restore_State
     (This      : in out Instance'Class;
      Stack_Top : Object);

   -------------------
   -- Add_Container --
   -------------------

   overriding procedure Add_Container
     (This      : in out Instance;
      Container : not null access Skit.Containers.Abstraction'Class)
   is
   begin
      This.Core.Add_Container (Container);
   end Add_Container;

   -----------
   -- Apply --
   -----------

   overriding procedure Apply
     (This : in out Instance)
   is
      Args : Object_Array (1 .. 2);
   begin
      if not This.Pop (Args) then
         raise Constraint_Error with
           "apply: not enough arguments";
      end if;
      This.Push (This.Core.Allocate (Args (1), Args (2)));
   end Apply;

   ----------
   -- Bind --
   ----------

   overriding function Bind
     (This      : in out Instance;
      Primitive : Skit.Primitives.Abstraction'Class)
      return Object
   is
   begin
      This.Prims.Append (Primitive);
      return (Object_Payload (This.Prims.Last_Index)
              + Primitive_Function_Payload'First,
              Primitive_Object);
   end Bind;

   ------------
   -- Create --
   ------------

   function Create
     (Core_Size  : Positive)
      return Skit.Machine.Reference
   is
      This : constant Reference := new Instance;
   begin
      This.Core := Skit.Impl.Memory.Create (Core_Size);
      This.Core.Add_Container (This);
      return Skit.Machine.Reference (This);
   end Create;

   --------------
   -- Evaluate --
   --------------

   overriding procedure Evaluate
     (This : in out Instance)
   is
      X : constant Object := This.Pop;
   begin
      if Trace then
         Ada.Text_IO.Put_Line
           ("evaluate: " & Debug.Image (X, This'Access) & " {");
      end if;
      case X.Tag is
         when Integer_Object =>
            This.Push (X);
         when Float_Object =>
            This.Push (X);
         when Primitive_Object =>
            This.Push (X);
         when Application_Object =>
            This.Push (Control, X);
            This.Evaluate_Application;
      end case;
      if Trace then
         Ada.Text_IO.Put_Line
           ("} result: " & Debug.Image (This.Top, This'Access));
      end if;
   end Evaluate;

   --------------------------
   -- Evaluate_Application --
   --------------------------

   procedure Evaluate_Application
     (This  : in out Instance'Class)
   is
      It      : Object;
      Changed : Boolean := True;

      procedure Update
        (X : Object;
         V : Object);

      ------------
      -- Update --
      ------------

      procedure Update
        (X : Object;
         V : Object)
      is
      begin
         This.Set_Left (X, I);
         This.Set_Right (X, V);
      end Update;

   begin
      while Changed loop
         Changed := False;
         It := This.Pop (Control);

         while It.Tag = Application_Object loop
            if Trace then
               Ada.Text_IO.Put_Line
                 ("push: "
                  & Debug.Image (This.Right (It), This'Access));
            end if;
            This.Push (Control, It);
            It := This.Core.Left (This.Top (Control));
         end loop;

         if Trace then
            Ada.Text_IO.Put_Line
              ("stop: " & Debug.Image (It, This'Access));
         end if;

         if It.Tag = Primitive_Object then
            This.Reductions := This.Reductions + 1;
            case It.Payload is
               when Payload_I =>
                  declare
                     X : Object_Array (1 .. 1);
                  begin
                     if This.Pop (Control, X) then
                        It := This.Core.Right (X (1));
                        This.Push (Control, It);
                        Changed := True;
                        This.R_I := @ + 1;
                     end if;
                  end;

               when Payload_K =>
                  declare
                     X : Object_Array (1 .. 2);
                  begin
                     if This.Pop (Control, X) then
                        It := This.Core.Right (X (2));
                        Update (X (1), It);
                        This.Push (Control, It);
                        Changed := True;
                        This.R_K := @ + 1;
                     end if;
                  end;

               when Payload_S =>
                  if This.Pop (Control, This.R (1 .. 3)) then
                     This.Push (This.Right (This.R (3)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Push (This.Right (This.R (2)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (1), It);
                     This.Push (Control, It);
                     Changed := True;
                     This.R_S := @ + 1;
                  end if;

               when Payload_B =>
                  if This.Pop (Control, This.R (1 .. 3)) then
                     This.Push (This.Right (This.R (3)));
                     This.Push (This.Right (This.R (2)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (1), It);
                     This.Push (Control, It);
                     Changed := True;
                     This.R_B := @ + 1;
                  end if;

               when Payload_C =>
                  if This.Pop (Control, This.R (1 .. 3)) then
                     This.Push (This.Right (This.R (3)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Push (This.Right (This.R (2)));
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (1), It);
                     This.Push (Control, It);
                     Changed := True;
                     This.R_C := @ + 1;
                  end if;

               when Payload_S_Prime =>
                  if This.Pop (Control, This.R (1 .. 4)) then
                     This.Push (This.Right (This.R (4)));
                     This.Push (This.Right (This.R (3)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Apply;
                     This.Push (This.Right (This.R (2)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (1), It);
                     This.Push (Control, It);
                     Changed := True;
                     This.R_Sp := @ + 1;
                  end if;

               when Payload_B_Star =>
                  if This.Pop (Control, This.R (1 .. 4)) then
                     This.Push (This.Right (This.R (4)));
                     This.Push (This.Right (This.R (3)));
                     This.Push (This.Right (This.R (2)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Apply;
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (1), It);
                     This.Push (Control, It);
                     Changed := True;
                     This.R_Bs := @ + 1;
                  end if;

               when Payload_C_Prime =>
                  if This.Pop (Control, This.R (1 .. 4)) then
                     This.Push (This.Right (This.R (4)));
                     This.Push (This.Right (This.R (3)));
                     This.Push (This.Right (This.R (1)));
                     This.Apply;
                     This.Apply;
                     This.Push (This.Right (This.R (2)));
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (1), It);
                     This.Push (Control, It);
                     Changed := True;
                     This.R_Cp := @ + 1;
                  end if;

               when Primitive_Function_Payload =>
                  declare
                     P : constant Natural :=
                           Natural
                             (It.Payload - Primitive_Function_Payload'First);
                  begin
                     if P <= This.Prims.Last_Index then
                        declare
                           Fn : constant Skit.Primitives.Abstraction'Class :=
                                  This.Prims (P);
                        begin
                           for I in 1 .. Fn.Argument_Count loop
                              declare
                                 A : constant Object :=
                                       This.Right (This.Pop (Control));
                              begin
                                 This.Reset_State (A);
                                 This.Evaluate;
                                 This.Restore_State (This.Top);
                                 This.Push (Secondary_Stack, This.Pop);
                              end;
                           end loop;

                           if Trace then
                              Ada.Text_IO.Put_Line
                                ("args "
                                 & Debug.Image
                                   (This.Internal (Secondary_Stack),
                                    This'Access));
                           end if;

                           for I in 1 .. Fn.Argument_Count loop
                              This.Push (This.Pop (Secondary_Stack));
                           end loop;

                           Fn.Evaluate (This);
                           This.Push (Control, This.Pop);

                           Changed := True;
                        end;
                     end if;
                  end;
               when others =>
                  null;
            end case;
         end if;
      end loop;

      This.Push (It);

      declare
         Count : Natural := 0;
      begin
         while This.Internal (Control) /= Nil loop
            This.Push (This.Core.Right (This.Pop (Control)));
            This.Apply;
            Count := Count + 1;
         end loop;
      end;

   end Evaluate_Application;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (This : in out Instance;
      Set  : not null access procedure (Marked_Object : in out Object))
   is
   begin
      for R of This.Internal loop
         Set (R);
      end loop;
      for R of This.R loop
         Set (R);
      end loop;
      for Local of This.Locals loop
         Set (Local);
      end loop;
      for Temp of This.Temps loop
         Set (Temp);
      end loop;
   end Mark;

   ---------
   -- Pop --
   ---------

   overriding function Pop
     (This   : in out Instance;
      Values : out Object_Array)
      return Boolean
   is
   begin
      return This.Pop (Stack, Values);
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop
     (This   : in out Instance'Class;
      Stack  : Internal_Register;
      Values : out Object_Array)
      return Boolean
   is
      S : Object := This.Internal (Stack);
   begin
      for I in reverse 1 .. Values'Length loop
         if S = Nil then
            return False;
         end if;
         This.Locals (I) := This.Core.Left (S);
         S := This.Core.Right (S);
      end loop;
      Values := This.Locals (1 .. Values'Length);
      This.Internal (Stack) := S;
      This.Locals := [others => Nil];
      return True;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop
     (This   : in out Instance'Class;
      Stack  : Internal_Register)
      return Object
   is
      V : Object_Array (1 .. 1);
   begin
      if not This.Pop (Stack, V) then
         raise Constraint_Error with "empty stack: " & Stack'Image;
      end if;
      return V (1);
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This   : in out Instance;
      Values : in out Object_Array)
   is
   begin
      This.Push (Stack, Values);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This   : in out Instance'Class;
      Stack  : Internal_Register;
      Values : in out Object_Array)
   is
   begin
      This.Locals (1 .. Values'Length) := Values;
      for V of This.Locals (1 .. Values'Length) loop
         This.Internal (Stack) :=
           This.Core.Allocate (V, This.Internal (Stack));
      end loop;
      Values := This.Locals (1 .. Values'Length);
      This.Locals := [others => Nil];
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (This  : in out Instance'Class;
      Stack : Internal_Register;
      Value : Object)
   is
      T : Object := Value;
   begin
      This.Internal (Stack) := This.Core.Allocate (T, This.Internal (Stack));
   end Push;

   ------------
   -- Report --
   ------------

   overriding procedure Report
     (This : Instance)
   is
   begin
      Ada.Text_IO.Put_Line ("reductions:" & This.Reductions'Image);
      Ada.Text_IO.Put_Line
        ("per combinator: "
         & "I" & This.R_I'Image
         & "; K" & This.R_K'Image
         & "; S" & This.R_S'Image
         & "; B" & This.R_B'Image
         & "; C" & This.R_C'Image
         & "; S'" & This.R_Sp'Image
         & "; B*" & This.R_Bs'Image
         & "; C'" & This.R_Cp'Image);
      This.Core.Report;
   end Report;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
     (This      : in out Instance'Class;
      Stack_Top : Object)
   is
   begin
      This.Push (Dump, This.Internal (Control));
      This.Push (Dump, This.Internal (Secondary_Stack));
      This.Push (Dump, This.Internal (Stack));
      This.Internal (Control) := Nil;
      This.Internal (Secondary_Stack) := Nil;
      This.Internal (Control) := Nil;
      This.Push (Stack, Stack_Top);
   end Reset_State;

   -------------------
   -- Restore_State --
   -------------------

   procedure Restore_State
     (This      : in out Instance'Class;
      Stack_Top : Object)
   is
   begin
      This.R (1) := Stack_Top;
      This.Internal (Stack) := This.Pop (Dump);
      This.Push (Stack, This.R (1));
      This.Internal (Secondary_Stack) := This.Pop (Dump);
      This.Internal (Control) := This.Pop (Dump);
   end Restore_State;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (This  : in out Instance;
      T     : Skit.Machine.Temporary;
      Value : Object)
   is
   begin
      This.Temps (T) := Value;
   end Set;

   --------------
   -- Set_Left --
   --------------

   overriding procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object)
   is
   begin
      This.Core.Set_Left (App, To);
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   overriding procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object)
   is
   begin
      This.Core.Set_Right (App, To);
   end Set_Right;

   ---------
   -- Top --
   ---------

   overriding function Top
     (This   : Instance)
      return Object
   is
   begin
      return This.Top (Stack);
   end Top;

   ---------
   -- Top --
   ---------

   function Top
     (This   : Instance'Class;
      Stack  : Internal_Register)
      return Object
   is
   begin
      return This.Core.Left (This.Internal (Stack));
   end Top;

end Skit.Impl.Machines;
