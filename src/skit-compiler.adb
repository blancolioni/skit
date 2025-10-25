package body Skit.Compiler is

   Do_Opt : constant Boolean := True;
   Indent : Natural := 0;

   procedure Abstract_Variable
     (Machine : Skit.Machine.Reference;
      Variable : Object);

   procedure Optimise
     (Machine : Skit.Machine.Reference);

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
            if Do_Opt then
               Optimise (Machine);
            end if;
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

   --------------
   -- Optimise --
   --------------

   procedure Optimise
     (Machine : Skit.Machine.Reference)
   is
      Top : constant Object := Machine.Top;
      function Is_S return Boolean
      is (Top.Tag = Application_Object
          and then Machine.Left (Top).Tag = Application_Object
          and then Machine.Left (Machine.Left (Top)) = S);

      function Is_App_K (O : Object) return Boolean
      is (O.Tag = Application_Object
          and then Machine.Left (O) = K);

      function App_K (O : Object) return Object
      is (Machine.Right (O));

      X : constant Object :=
            (if Is_S then Machine.Right (Machine.Left (Top)) else Nil);
      Y : constant Object :=
            (if Is_S
             then Machine.Right (Top)
             else Nil);
   begin
      if Is_S then
         if Is_App_K (X) then
            if Is_App_K (Y) then
               Machine.Set (0, App_K (X));
               Machine.Set (1, App_K (Y));
               Machine.Drop;
               Machine.Push (K);
               Machine.Push (Machine.Get (0));
               Machine.Push (Machine.Get (1));
               Machine.Apply;
               Machine.Apply;
            elsif Y = I then
               Machine.Set (0, App_K (X));
               Machine.Drop;
               Machine.Push (Machine.Get (0));
            else
               Machine.Set (0, App_K (X));
               Machine.Set (1, Y);
               Machine.Drop;
               Machine.Push (B);
               Machine.Push (Machine.Get (0));
               Machine.Apply;
               Machine.Push (Machine.Get (1));
               Machine.Apply;
            end if;
         elsif Is_App_K (Y) then
               Machine.Set (0, X);
               Machine.Set (1, App_K (Y));
               Machine.Drop;
               Machine.Push (C);
               Machine.Push (Machine.Get (0));
               Machine.Apply;
               Machine.Push (Machine.Get (1));
               Machine.Apply;
         end if;
      end if;
   end Optimise;

end Skit.Compiler;
