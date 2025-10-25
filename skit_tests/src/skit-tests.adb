with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with Skit.Compiler;
with Skit.Debug;
with Skit.Environment;
with Skit.Impl;
with Skit.Library;
with Skit.Machine;
with Skit.Parser;
with Skit.Stacks;

package body Skit.Tests is

   Machine : Skit.Machine.Reference;
   Env     : Skit.Environment.Reference;

   package Variable_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

   package Source_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object;
      Compile    : Boolean);

   Vrbs    : Variable_Vectors.Vector;

   Prelude : constant Source_Lists.List :=
               ["!id \x.x",
                "!false \x.\y.y",
                "!true \x.\y.x",
                "!if \c.\t.\f.c t f",
                "!Y S S I (C B (S I I))",
                "!succ \x.+ x 1",
                "!pred \x.- x 1",
                "!not \x.if x false true",
                "!zero \x.eq x 0 true false",
                "!count Y (\f.\n.zero n 0 (f (- n 1)))",
                "!fac Y (\f.\n.zero n 1 (* n (f (- n 1))))",
                "!gcd Y (\f.\a.\b.if (zero b) a (f b (mod a b)))",
                "!rec Y (\f.\x.eq x 0 22 (f (pred x)))"
               ];

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      function To_Object (Tok : String) return Skit.Object;
      procedure Bind_Value (Name : String);

      ----------------
      -- Bind_Value --
      ----------------

      procedure Bind_Value (Name : String) is
      begin
         Env.Bind (Name, Machine.Pop);
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
            declare
               Value : constant Object := Env.Lookup (Tok);
            begin
               if Value /= Undefined then
                  Ada.Text_IO.Put_Line
                    (Tok & ": " & Skit.Debug.Image (Value, Machine));
                  return Value;
               end if;
            end;

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
      Env     := Skit.Environment.Create (Machine);
      Skit.Library.Load_Standard_Library (Env);

      for Decl of Prelude loop
         Skit.Parser.Parse
           (Decl, To_Object'Access, Bind_Value'Access, Machine);
      end loop;

   end Initialize;

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
         Env.Bind (Name, Machine.Pop);

         --  declare
         --     Rec : constant Binding_Record :=
         --             (Name'Length, Name, Machine.Pop);
         --  begin
         --     if Trace then
         --        Ada.Text_IO.Put_Line
         --          (Name & " <- " & Debug.Image (Rec.Value, Machine));
         --     end if;
         --     Env.List.Append (Rec);
         --  end;
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
            declare
               Value : constant Object := Env.Lookup (Tok);
            begin
               if Value /= Undefined then
                  return Value;
               end if;
            end;

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
