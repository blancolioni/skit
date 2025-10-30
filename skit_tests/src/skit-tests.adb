with Ada.Text_IO;

with Skit.Compiler;
with Skit.Debug;
with Skit.Environment;
with Skit.Impl;
with Skit.Library;
with Skit.Machine;
with Skit.Stacks;

package body Skit.Tests is

   Machine : Skit.Machine.Reference;
   Env     : Skit.Environment.Reference;

   Total, Pass, Fail : Natural := 0;

   procedure Put
     (S   : String;
      Max : Natural);

   ---------
   -- Put --
   ---------

   procedure Put
     (S   : String;
      Max : Natural)
   is
   begin
      if S'Length > Max then
         Ada.Text_IO.Put (S (S'First .. S'First + Max - 1));
      else
         Ada.Text_IO.Put (S);
      end if;
   end Put;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object;
      Compile    : Boolean);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Machine := Skit.Impl.Machine (16 * 1024);
      Env     := Skit.Environment.Create (Machine);
      Skit.Library.Load_Standard_Library (Env);
   end Initialize;

   ------------
   -- Report --
   ------------

   procedure Report is
   begin
      Ada.Text_IO.Put_Line
        ("Total tests" & Total'Image
        & "; passed: "
        & Pass'Image
        & "; failed: "
        & Fail'Image);

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
      Total := @ + 1;

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
         Put (Name, 38);

         if Result = Expected then
            Pass := @ + 1;
            Ada.Text_IO.Put_Line ("PASS");
         else
            Fail := @ + 1;
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
   begin
      Total := @ + 1;
      Env.Evaluate (Source);

      declare
         Result : constant Skit.Object := Machine.Pop;
      begin
         Put (Source, 38);
         Ada.Text_IO.Set_Col (40);

         if Result = Expected then
            Ada.Text_IO.Put_Line ("PASS");
            Pass := @ + 1;
         else
            Ada.Text_IO.Put_Line
              ("expected: " & Skit.Debug.Image (Expected)
               & "; found " & Skit.Debug.Image (Result, Machine));
            Fail := @ + 1;
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
