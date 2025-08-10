with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Skit.Compiler;
with Skit.Debug;
with Skit.Impl;
with Skit.Machine;
with Skit.Parser;
with Skit.Primitives;

package body Skit.Tests is

   package Variable_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

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

   overriding function Evaluate
     (This      : Prim_Int_Op_Instance;
      Arguments : Object_Array)
      return Object
   is (To_Object
       (This.Op
          (To_Integer (Arguments (1)),
             To_Integer (Arguments (2)))));

   function Add (X, Y : Integer) return Integer is (X + Y);

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object;
      Compile    : Boolean);

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
      Add_Fn  : constant Object :=
                  Machine.Bind (Prim_Int_Op_Instance'(Op => Add'Access));

      Vrbs    : Variable_Vectors.Vector;

      function To_Object (Tok : String) return Skit.Object;

      ---------------
      -- To_Object --
      ---------------

      function To_Object (Tok : String) return Skit.Object is
      begin
         if (for all Ch of Tok => Ch in '0' .. '9') then
            return To_Object (Natural'Value (Tok));
         elsif Tok = "+" then
            return Add_Fn;
         else
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
      Skit.Parser.Parse (Source, To_Object'Access, Machine);
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
