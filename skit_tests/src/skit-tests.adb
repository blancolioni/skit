with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Skit.Compiler;
with Skit.Debug;
with Skit.Impl;
with Skit.Machine;
with Skit.Parser;
with Skit.Primitives;

package body Skit.Tests is

   Trace : constant Boolean := False;

   type Binding_Record (Length : Natural) is
      record
         Name  : String (1 .. Length);
         Value : Object;
      end record;

   package Binding_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Binding_Record);

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

   overriding function Argument_Count
     (This : Prim_Int_Op_Instance)
      return Natural
   is (2);

   overriding function Is_Lazy
     (This  : Prim_Int_Op_Instance;
      Index : Positive)
      return Boolean
   is (False);

   overriding function Evaluate
     (This      : Prim_Int_Op_Instance;
      Arguments : Object_Array)
      return Object;

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

   overriding function Argument_Count
     (This : Predicate_Instance)
      return Natural
   is (4);

   overriding function Is_Lazy
     (This  : Predicate_Instance;
      Index : Positive)
      return Boolean
   is (Index > 2);

   overriding function Evaluate
     (This      : Predicate_Instance;
      Arguments : Object_Array)
      return Object;

   function Eq (X, Y : Object) return Boolean is (X = Y);

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object;
      Compile    : Boolean);

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Predicate_Instance;
      Arguments : Object_Array)
      return Object
   is
   begin
      return (if This.Op (Arguments (1), Arguments (2))
              then Arguments (3)
              else Arguments (4));
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Prim_Int_Op_Instance;
      Arguments : Object_Array)
      return Object
   is
   begin
      return X : constant Object :=
        To_Object
          (This.Op
             (To_Integer (Arguments (1)),
              To_Integer (Arguments (2))))
      do
         if Trace then
            Ada.Text_IO.Put_Line
              ("f " & Debug.Image (Arguments (1))
               & " " & Debug.Image (Arguments (2))
               & " ==> "
               & Debug.Image (X));
         end if;
      end return;
   end Evaluate;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object;
      Compile    : Boolean)
   is
      Machine : constant Skit.Machine.Reference :=
                  Skit.Impl.Machine (8 * 1024);
      Add_Fn  : constant Object :=
                  Machine.Bind (Prim_Int_Op_Instance'(Op => Add'Access))
        with Unreferenced;
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
      Machine : constant Skit.Machine.Reference :=
                  Skit.Impl.Machine (8 * 1024);
      Env     : Binding_Lists.List;
      Vrbs    : Variable_Vectors.Vector;

      Prelude : constant Source_Lists.List :=
                  ["!id \x.x",
                   "!false \x.\y.y",
                   "!true \x.\y.x",
                   "!if \c.\t.\f.c t f",
                   "!Y \f.(\x.f (x x)) (\x.f (x x))",
                   "!succ \x.+ x 1",
                   "!pred \x.- x 1",
                   "!not \x.if x false true",
                   "!zero \x.eq x 0 true false",
                   "!fac Y(\f.\n.zero n 1 (* n (f (- n 1))))",
                   "!gcd Y (\f.\a.\b.if (zero b) a (f b (mod a b)))"
                  ];

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
            Env.Append (Rec);
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
         Env.Append
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
         Env.Append
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
         else
            for Decl of Env loop
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
      Primitive ("+", Add'Access);
      Primitive ("-", Sub'Access);
      Primitive ("*", Mul'Access);
      Primitive ("mod", Modulus'Access);
      Primitive ("eq", Eq'Access);

      for Decl of Prelude loop
         Skit.Parser.Parse
           (Decl, To_Object'Access, Bind_Value'Access, Machine);
      end loop;

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
