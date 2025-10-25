with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Skit.Compiler;
with Skit.Containers;
with Skit.Debug;
with Skit.Impl;
with Skit.Machine;
with Skit.Parser;
with Skit.Primitives;
with Skit.Stacks;

package body Skit.Tests is

   Trace : constant Boolean := False;

   Machine : Skit.Machine.Reference;

   type Binding_Record (Length : Natural) is
      record
         Name  : String (1 .. Length);
         Value : Object;
      end record;

   package Binding_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Binding_Record);

   type Environment is new Skit.Containers.Abstraction with
      record
         List : Binding_Lists.List;
      end record;

   overriding procedure Mark
     (This : in out Environment;
      Set  : not null access
        procedure (Item : in out Object));

   package Variable_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

   package Source_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Prim_Int_Op is access
     function (X, Y : Integer) return Integer;

   type Prim_Int_Op_Instance is
     new Skit.Primitives.Abstraction with
      record
         Op : Prim_Int_Op;
      end record;

   overriding function Name
     (This : Prim_Int_Op_Instance)
      return String
   is ("prim-int-op");

   overriding function Argument_Count
     (This : Prim_Int_Op_Instance)
      return Natural
   is (2);

   overriding procedure Evaluate
     (This    : Prim_Int_Op_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class);

   function Add (X, Y : Integer) return Integer is (X + Y);
   function Sub (X, Y : Integer) return Integer is (X - Y);
   function Mul (X, Y : Integer) return Integer is (X * Y);
   function Modulus (X, Y : Integer) return Integer is (X mod Y);

   type Predicate_Op is access
     function (X, Y : Object) return Boolean;

   type Predicate_Instance is
     new Skit.Primitives.Abstraction with
      record
         Op : Predicate_Op;
      end record;

   overriding function Name
     (This : Predicate_Instance)
      return String
   is ("predicate");

   overriding function Argument_Count
     (This : Predicate_Instance)
      return Natural
   is (2);

   overriding procedure Evaluate
     (This    : Predicate_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class);

   function Eq (X, Y : Object) return Boolean is (X = Y);

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object;
      Compile    : Boolean);

   Env     : aliased Environment;
   Vrbs    : Variable_Vectors.Vector;

   Prelude : constant Source_Lists.List :=
               ["!id \x.x",
                "!false \x.\y.y",
                "!true \x.\y.x",
                "!if \c.\t.\f.c t f",
                "!U S I I",
                "!Y \f.(\x.f (x x)) (\x.f (x x))",
                "!succ \x.+ x 1",
                "!pred \x.- x 1",
                "!not \x.if x false true",
                "!zero \x.eq x 0 true false",
                "!count Y (\f.\n.zero n 0 (f (- n 1)))",
                "!fac Y (\f.\n.zero n 1 (* n (f (- n 1))))",
                "!gcd Y (\f.\a.\b.if (zero b) a (f b (mod a b)))",
                "!rec Y (\f.\x.eq x 0 22 (f (pred x)))"
               ];

   --------------
   -- Evaluate --
   --------------

   overriding procedure Evaluate
     (This    : Predicate_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class)
   is
      X : constant Object := Stack.Pop;
      Y : constant Object := Stack.Pop;
   begin
      if This.Op (X, Y) then
         Stack.Push (Skit.K);
      else
         Stack.Push (Skit.K);
         Stack.Push (Skit.I);
         Stack.Apply;
      end if;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding procedure Evaluate
     (This    : Prim_Int_Op_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class)
   is
      X : constant Object := Stack.Pop;
      Y : constant Object := Stack.Pop;
      Z : constant Object :=
        To_Object
          (This.Op
             (To_Integer (X),
              To_Integer (Y)));
   begin
      Stack.Push (Z);
   end Evaluate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      procedure Primitive
        (Name : String;
         Fn   : Prim_Int_Op);

      procedure Primitive
        (Name : String;
         Fn   : Predicate_Op);

      function To_Object (Tok : String) return Skit.Object;
      procedure Bind_Value (Name : String);

      ----------------
      -- Bind_Value --
      ----------------

      procedure Bind_Value (Name : String) is
      begin
         --  Skit.Compiler.Compile (Machine);
         declare
            Rec : constant Binding_Record := (Name'Length, Name, Machine.Pop);
         begin
            if Trace then
               Ada.Text_IO.Put_Line
                 (Name & " <- " & Debug.Image (Rec.Value, Machine));
            end if;
            Env.List.Append (Rec);
         end;
      end Bind_Value;

      ---------------
      -- Primitive --
      ---------------

      procedure Primitive
        (Name : String;
         Fn   : Prim_Int_Op)
      is
      begin
         Env.List.Append
           (Binding_Record'
              (Name'Length, Name,
               Machine.Bind (Prim_Int_Op_Instance'(Op => Fn))));
      end Primitive;

      ---------------
      -- Primitive --
      ---------------

      procedure Primitive
        (Name : String;
         Fn   : Predicate_Op)
      is
      begin
         Env.List.Append
           (Binding_Record'
              (Name'Length, Name,
               Machine.Bind (Predicate_Instance'(Op => Fn))));
      end Primitive;

      ---------------
      -- To_Object --
      ---------------

      function To_Object (Tok : String) return Skit.Object is
      begin
         if (for all Ch of Tok => Ch in '0' .. '9') then
            return To_Object (Natural'Value (Tok));
         elsif Tok = "I" then
            return Skit.I;
         elsif Tok = "S" then
            return Skit.S;
         elsif Tok = "K" then
            return Skit.K;
         elsif Tok = "B" then
            return Skit.B;
         elsif Tok = "C" then
            return Skit.C;
         else
            for Decl of Env.List loop
               if Decl.Name = Tok then
                  return Decl.Value;
               end if;
            end loop;

            declare
               use Variable_Vectors;
               Position : constant Cursor :=
                            Vrbs.Find (Tok);
            begin
               if Has_Element (Position) then
                  return (Primitive_Variable_Payload'First
                          + Object_Payload (To_Index (Position)),
                          Primitive_Object);
               else
                  Vrbs.Append (Tok);
                  return (Primitive_Variable_Payload'First
                          + Object_Payload (Vrbs.Last_Index),
                          Primitive_Object);
               end if;
            end;
         end if;
      end To_Object;

   begin
      Machine := Skit.Impl.Machine (4 * 1024);
      Primitive ("+", Add'Access);
      Primitive ("-", Sub'Access);
      Primitive ("*", Mul'Access);
      Primitive ("mod", Modulus'Access);
      Primitive ("eq", Eq'Access);

      Machine.Add_Container (Env'Access);

      for Decl of Prelude loop
         Skit.Parser.Parse
           (Decl, To_Object'Access, Bind_Value'Access, Machine);
      end loop;

   end Initialize;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (This : in out Environment;
      Set  : not null access
        procedure (Item : in out Object))
   is
   begin
      for Binding of This.List loop
         Set (Binding.Value);
      end loop;
   end Mark;

   ------------
   -- Report --
   ------------

   procedure Report is
   begin
      Machine.Report;
   end Report;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object;
      Compile    : Boolean)
   is
   begin
      for Element of Operations loop
         case Element.Op is
            when Apply =>
               Machine.Apply;
            when Push =>
               Machine.Push (Element.X);
         end case;
      end loop;

      if Compile then
         Skit.Compiler.Compile (Machine);
      end if;

      Machine.Evaluate;

      declare
         Result : constant Skit.Object := Machine.Pop;
      begin
         Ada.Text_IO.Put (Name);
         Ada.Text_IO.Set_Col (40);

         if Result = Expected then
            Ada.Text_IO.Put_Line ("PASS");
         else
            Ada.Text_IO.Put_Line
              ("expected: " & Skit.Debug.Image (Expected)
               & "; found " & Skit.Debug.Image (Result, Machine));
         end if;
      end;

   end Test;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object)
   is
   begin
      Test (Name, Operations, Expected, False);
   end Test;

   ----------
   -- Test --
   ----------

   procedure Test
     (Source   : String;
      Expected : Object)
   is
      function To_Object (Tok : String) return Skit.Object;
      procedure Bind_Value (Name : String);

      ----------------
      -- Bind_Value --
      ----------------

      procedure Bind_Value (Name : String) is
      begin
         --  Skit.Compiler.Compile (Machine);
         declare
            Rec : constant Binding_Record := (Name'Length, Name, Machine.Pop);
         begin
            if Trace then
               Ada.Text_IO.Put_Line
                 (Name & " <- " & Debug.Image (Rec.Value, Machine));
            end if;
            Env.List.Append (Rec);
         end;
      end Bind_Value;

      ---------------
      -- To_Object --
      ---------------

      function To_Object (Tok : String) return Skit.Object is
      begin
         if (for all Ch of Tok => Ch in '0' .. '9') then
            return To_Object (Natural'Value (Tok));
         elsif Tok = "I" then
            return Skit.I;
         elsif Tok = "S" then
            return Skit.S;
         elsif Tok = "K" then
            return Skit.K;
         elsif Tok = "B" then
            return Skit.B;
         elsif Tok = "C" then
            return Skit.C;
         else
            for Decl of Env.List loop
               if Decl.Name = Tok then
                  return Decl.Value;
               end if;
            end loop;

            declare
               use Variable_Vectors;
               Position : constant Cursor :=
                            Vrbs.Find (Tok);
            begin
               if Has_Element (Position) then
                  return (Primitive_Variable_Payload'First
                          + Object_Payload (To_Index (Position)),
                          Primitive_Object);
               else
                  Vrbs.Append (Tok);
                  return (Primitive_Variable_Payload'First
                          + Object_Payload (Vrbs.Last_Index),
                          Primitive_Object);
               end if;
            end;
         end if;
      end To_Object;

   begin
      Skit.Parser.Parse (Source, To_Object'Access, Bind_Value'Access, Machine);
      Skit.Compiler.Compile (Machine);
      Machine.Evaluate;

      declare
         Result : constant Skit.Object := Machine.Pop;
      begin
         Ada.Text_IO.Put (Source & " ==> " & Skit.Debug.Image (Expected));
         Ada.Text_IO.Set_Col (40);

         if Result = Expected then
            Ada.Text_IO.Put_Line ("PASS");
         else
            Ada.Text_IO.Put_Line
              ("expected: " & Skit.Debug.Image (Expected)
               & "; found " & Skit.Debug.Image (Result, Machine));
         end if;
      end;

   end Test;

   ----------
   -- Test --
   ----------

   procedure Test
     (Source   : String;
      Expected : Integer)
   is
   begin
      Test (Source, Skit.To_Object (Expected));
   end Test;

   -------------------
   -- Test_Compiler --
   -------------------

   procedure Test_Compiler
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object)
   is
   begin
      Test (Name, Operations, Expected, True);
   end Test_Compiler;

end Skit.Tests;
