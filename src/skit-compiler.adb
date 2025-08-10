package body Skit.Compiler is

   Indent : Natural := 0;

   procedure Abstract_Variable
     (Machine : Skit.Machine.Reference;
      Variable : Object);

   -----------------------
   -- Abstract_Variable --
   -----------------------

   procedure Abstract_Variable
     (Machine : Skit.Machine.Reference;
      Variable : Object)
   is
      Top : constant Object := Machine.Pop;
   begin
      Indent := Indent + 2;
      case Top.Tag is
         when Application_Object =>
            Machine.Set (0, Machine.Left (Top));
            Machine.Set (1, Machine.Right (Top));
            Machine.Push (Machine.Get (1));
            Machine.Push (Machine.Get (0));
            Abstract_Variable (Machine, Variable);
            Machine.Set (0, Machine.Pop);
            Machine.Set (1, Machine.Pop);
            Machine.Push (Machine.Get (0));
            Machine.Push (Machine.Get (1));
            Abstract_Variable (Machine, Variable);
            Machine.Set (0, Machine.Pop);
            Machine.Set (1, Machine.Pop);
            Machine.Push (Skit.S);
            Machine.Push (Machine.Get (1));
            Machine.Apply;
            Machine.Push (Machine.Get (0));
            Machine.Apply;
         when others =>
            if Top = Variable then
               Machine.Push (I);
            else
               Machine.Push (K);
               Machine.Push (Top);
               Machine.Apply;
            end if;
      end case;
      Indent := Indent - 2;
   end Abstract_Variable;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Machine : Skit.Machine.Reference)
   is
      Top : constant Object := Machine.Top;
   begin
      Indent := Indent + 2;
      case Top.Tag is
         when Integer_Object =>
            null;
         when Primitive_Object =>
            null;
         when Application_Object =>
            if Machine.Left (Top) = λ then
               declare
                  Variable : constant Object :=
                               Machine.Left (Machine.Right (Top));
                  Expression : constant Object :=
                                 Machine.Right (Machine.Right (Top));
               begin
                  Machine.Drop;
                  Machine.Push (Expression);
                  Compile (Machine);
                  Abstract_Variable (Machine, Variable);
               end;
            else
               Machine.Set (0, Machine.Left (Top));
               Machine.Set (1, Machine.Right (Top));
               Machine.Drop;
               Machine.Push (Machine.Get (1));
               Machine.Push (Machine.Get (0));
               Compile (Machine);
               Machine.Set (0, Machine.Pop);
               Machine.Set (1, Machine.Pop);
               Machine.Push (Machine.Get (0));
               Machine.Push (Machine.Get (1));
               Compile (Machine);
               Machine.Apply;
            end if;
         when Float_Object =>
            null;
      end case;
      Indent := Indent - 2;
   end Compile;

end Skit.Compiler;
