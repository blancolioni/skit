with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Skit.Allocator;
with Skit.Debug;
with Skit.Impl.Memory;
with Skit.Marks;
with Skit.Primitives;

package body Skit.Impl.Machines is

   Trace : constant Boolean := False;

   package Primitive_Function_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Natural, Skit.Primitives.Abstraction'Class, Skit.Primitives."=");

   subtype Register is Positive range 1 .. 15;
   Max_Locals : constant := 16;

   type Temporary_Array is array (Skit.Machine.Temporary) of Object;

   subtype Parent is Skit.Machine.Abstraction;
   type Instance is new Parent and Skit.Marks.Abstraction with
      record
         Core    : Skit.Allocator.Reference;
         Stack   : Object := Nil;
         Control : Object := Nil;
         SS      : Object := Nil;
         R       : Object_Array (Register);
         Locals  : Object_Array (1 .. Max_Locals) := [others => Nil];
         Temps   : Temporary_Array := [others => Nil];
         Prims   : Primitive_Function_Vectors.Vector;
      end record;
   type Reference is access all Instance'Class;

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
      return Object
   is (This.Core.Left (This.Stack));

   overriding procedure Set
     (This  : in out Instance;
      T     : Skit.Machine.Temporary;
      Value : Object);

   overriding function Get
     (This  : Instance;
      T     : Skit.Machine.Temporary)
      return Object
   is (This.Temps (T));

   function Pop_Control
     (This   : in out Instance'Class;
      Values : out Object_Array)
      return Boolean;

   function Pop_Control
     (This   : in out Instance'Class)
      return Object;

   function Control_Top
     (This   : Instance'Class)
      return Object;

   procedure Push_Control
     (This  : in out Instance'Class;
      Value : Object);

   procedure Evaluate_Application
     (This  : in out Instance'Class);

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

   -----------------
   -- Control_Top --
   -----------------

   function Control_Top
     (This   : Instance'Class)
      return Object
   is
   begin
      return This.Core.Left (This.Control);
   end Control_Top;

   ------------
   -- Create --
   ------------

   function Create
     (Core_Size  : Positive)
      return Skit.Machine.Reference
   is
      This : constant Reference := new Instance;
   begin
      This.Core := Skit.Impl.Memory.Create (Core_Size, This);
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
           ("evaluate: " & Debug.Image (X, This'Access));
      end if;
      case X.Tag is
         when Integer_Object =>
            This.Push (X);
         when Float_Object =>
            This.Push (X);
         when Primitive_Object =>
            This.Push (X);
         when Application_Object =>
            This.Push_Control (X);
            This.Evaluate_Application;
      end case;
      if Trace then
         Ada.Text_IO.Put_Line
           ("result: " & Debug.Image (This.Top, This'Access));
      end if;
   end Evaluate;

   --------------------------
   -- Evaluate_Application --
   --------------------------

   procedure Evaluate_Application
     (This  : in out Instance'Class)
   is
      It : Object;
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
         It := This.Pop_Control;

         while It.Tag = Application_Object loop
            if Trace then
               Ada.Text_IO.Put_Line
                 ("push: " & Debug.Image (It, This'Access));
            end if;
            This.Push_Control (It);
            It := This.Core.Left (This.Control_Top);
         end loop;

         if Trace then
            Ada.Text_IO.Put_Line
              ("stop: " & Debug.Image (It, This'Access));
         end if;

         if It.Tag = Primitive_Object then
            case It.Payload is
               when Payload_I =>
                  declare
                     X : Object_Array (1 .. 1);
                  begin
                     if This.Pop_Control (X) then
                        It := This.Core.Right (X (1));
                        This.Push_Control (It);
                        Changed := True;
                     end if;
                  end;

               when Payload_K =>
                  declare
                     X : Object_Array (1 .. 2);
                  begin
                     if This.Pop_Control (X) then
                        It := This.Core.Right (X (1));
                        Update (X (2), It);
                        This.Push_Control (It);
                        Changed := True;
                     end if;
                  end;

               when Payload_S =>
                  if This.Pop_Control (This.R (1 .. 3)) then
                     This.Push (This.Right (This.R (1)));
                     This.Push (This.Right (This.R (3)));
                     This.Apply;
                     This.Push (This.Right (This.R (2)));
                     This.Push (This.Right (This.R (3)));
                     This.Apply;
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (3), It);
                     This.Push_Control (It);
                     Changed := True;
                  end if;

               when Payload_B =>
                  if This.Pop_Control (This.R (1 .. 3)) then
                     This.Push (This.Right (This.R (1)));
                     This.Push (This.Right (This.R (2)));
                     This.Push (This.Right (This.R (3)));
                     This.Apply;
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (3), It);
                     This.Push_Control (It);
                     Changed := True;
                  end if;

               when Payload_C =>
                  if This.Pop_Control (This.R (1 .. 3)) then
                     This.Push (This.Right (This.R (1)));
                     This.Push (This.Right (This.R (3)));
                     This.Apply;
                     This.Push (This.Right (This.R (2)));
                     This.Apply;
                     It := This.Pop;
                     Update (This.R (3), It);
                     This.Push_Control (It);
                     Changed := True;
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
                           Arg_Count : constant Natural :=
                                         Fn.Argument_Count;
                           Args      : Object_Array renames
                                         This.R (1 .. Arg_Count);
                           Arg       : Object;
                        begin
                           if This.Pop_Control (Args) then
                              This.Push (Args (Arg_Count));
                              This.Push (Args);
                              for I in reverse 1 .. Arg_Count loop
                                 This.Push (This.Right (This.Pop));
                                 if not Fn.Is_Lazy (I) then
                                    This.Evaluate;
                                 end if;
                                 Arg := This.Pop;
                                 if Trace then
                                    Ada.Text_IO.Put_Line
                                      ("arg" & Integer'Image (-I)
                                       & ": "
                                       & Debug.Image (Arg, This'Access));
                                 end if;
                                 This.SS := This.Core.Allocate (Arg, This.SS);
                              end loop;
                              for R of Args loop
                                 R := This.Left (This.SS);
                                 This.SS := This.Right (This.SS);
                              end loop;
                              It := Fn.Evaluate (Args);
                              declare
                                 L : constant Object := This.Pop;
                              begin
                                 This.Set_Left (L, I);
                                 This.Set_Right (L, It);
                              end;
                              This.Push_Control (It);
                              Changed := True;
                           end if;
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
         while This.Control /= Nil loop
            This.Push (This.Core.Right (This.Pop_Control));
            This.Apply;
            Count := Count + 1;
         end loop;
         --  for I in 1 .. Count loop
         --     This.Apply;
         --  end loop;
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
      Set (This.Stack);
         Set (This.Control);
         Set (This.SS);
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
      S : Object := This.Stack;
   begin
      for I in reverse 1 .. Values'Length loop
         if S = Nil then
            return False;
         end if;
         This.Locals (I) := This.Core.Left (S);
         S := This.Core.Right (S);
      end loop;
      Values := This.Locals (1 .. Values'Length);
      This.Stack := S;
      This.Locals := [others => Nil];
      return True;
   end Pop;

   -----------------
   -- Pop_Control --
   -----------------

   function Pop_Control
     (This   : in out Instance'Class;
      Values : out Object_Array)
      return Boolean
   is
      S : Object := This.Control;
   begin
      for I in 1 .. Values'Length loop
         if S = Nil then
            return False;
         end if;
         Values (I) := This.Core.Left (S);
         S := This.Core.Right (S);
      end loop;
      This.Control := S;
      return True;
   end Pop_Control;

   -----------------
   -- Pop_Control --
   -----------------

   function Pop_Control
     (This   : in out Instance'Class)
      return Object
   is
      V : Object_Array (1 .. 1);
   begin
      if not This.Pop_Control (V) then
         raise Constraint_Error with "Control stack empty";
      end if;
      return V (1);
   end Pop_Control;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This   : in out Instance;
      Values : in out Object_Array)
   is
   begin
      This.Locals (1 .. Values'Length) := Values;
      for V of This.Locals (1 .. Values'Length) loop
         This.Stack := This.Core.Allocate (V, This.Stack);
      end loop;
      Values := This.Locals (1 .. Values'Length);
      This.Locals := [others => Nil];
   end Push;

   ------------------
   -- Push_Control --
   ------------------

   procedure Push_Control
     (This  : in out Instance'Class;
      Value : Object)
   is
      T : Object := Value;
   begin
      This.Control := This.Core.Allocate (T, This.Control);
   end Push_Control;

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

end Skit.Impl.Machines;
