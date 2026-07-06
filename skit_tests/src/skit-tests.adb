with Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
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

   type Null_Resolver is new Skit.Terms.Resolver_Interface with null record;

   overriding function Resolve
     (This : Null_Resolver;
      Name : String)
      return Skit.Object
   is (raise Constraint_Error with "undefined: " & Name);

   Local_Resolver : aliased Null_Resolver;

   procedure Put
     (S   : String;
      Max : Natural);

   function Load (Ops : Stack_Operation_Array) return Skit.Terms.Term;

   ----------
   -- Load --
   ----------

   function Load (Ops : Stack_Operation_Array) return Skit.Terms.Term is

      package Term_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Skit.Terms.Term, Skit.Terms."=");

      Stack : Term_Lists.List;

      function Pop return Skit.Terms.Term;
      procedure Push (X : Skit.Terms.Term);

      ---------
      -- Pop --
      ---------

      function Pop return Skit.Terms.Term is
      begin
         return T : constant Skit.Terms.Term := Stack.Last_Element do
            Stack.Delete_Last;
         end return;
      end Pop;

      ----------
      -- Push --
      ----------

      procedure Push (X : Skit.Terms.Term) is
      begin
         Stack.Append (X);
      end Push;

   begin
      for Item of Ops loop
         case Item.Op is
            when Push =>
               Push (Item.X);
            when Apply =>
               declare
                  Right : constant Skit.Terms.Term := Pop;
                  Left  : constant Skit.Terms.Term := Pop;
               begin
                  Push (Skit.Terms.Apply (Left, Right));
               end;
            when Lambda =>
               declare
                  Lambda_Body : constant Skit.Terms.Term := Pop;
                  Lambda_Var  : constant Skit.Terms.Term := Pop;
               begin
                  Push
                    (Skit.Terms.Lambda
                       (Skit.Terms.Get_Symbol (Lambda_Var),
                        Lambda_Body));
               end;
         end case;
      end loop;

      return Pop;
   end Load;

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
      Machine := Skit.Impl.Machine (256 * 1024);
      Env     := Skit.Environment.Create (Machine);
      Skit.Library.Load_Standard_Library (Env);
   end Initialize;

   ----------
   -- Prim --
   ----------

   function Prim (P : Natural) return Stack_Operation_Type is
   begin
      return Push
        (Object'(Object_Payload (P + 64), Primitive_Object));
   end Prim;

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
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Fail));
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

      declare
         Term : Skit.Terms.Term := Load (Operations);
      begin
         if Compile then
            Term := Skit.Compiler.Compile (Term);
         end if;

         Machine.Push
           (Skit.Terms.Install (Term, Local_Resolver'Access, Machine));
      end;

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

   ---------
   -- Var --
   ---------

   function Var (V : String) return Stack_Operation_Type is
   begin
      return Push (Skit.Terms.Symbol (V));
   end Var;

end Skit.Tests;
