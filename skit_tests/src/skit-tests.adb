with Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;

with Skit.Compiler;
with Skit.Handles;
with Skit.Parser;

package body Skit.Tests is

   Handle : Skit.Handles.Handle;

   Total, Pass, Fail : Natural := 0;

   procedure Bind (Name : String; Term : Skit.Terms.Term);

   function Resolve
     (Name : String)
      return Skit.Object;

   procedure Put
     (S   : String;
      Max : Natural);

   function Load (Ops : Stack_Operation_Array) return Skit.Terms.Term;

   pragma Warnings (Off);

   function Evaluate_Add
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is (To_Object (To_Integer (Arguments (1)) + To_Integer (Arguments (2))));

   function Evaluate_Sub
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array) return Object
   is (To_Object (To_Integer (Arguments (1)) - To_Integer (Arguments (2))));

   function Evaluate_Mul
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array) return Object
   is (To_Object (To_Integer (Arguments (1)) * To_Integer (Arguments (2))));

   function Evaluate_Div
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is (To_Object (To_Integer (Arguments (1)) / To_Integer (Arguments (2))));

   function Evaluate_Mod
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is (To_Object (To_Integer (Arguments (1)) mod To_Integer (Arguments (2))));

   function Evaluate_Eq
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is (To_Object
       (if Arguments (1) = Arguments (2)
          then 1 else 0));

   function Evaluate_Choose
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array) return Object
   is (if Arguments (1) = To_Object (0)
       then Arguments (2)
       else Arguments (3));

   function Evaluate_Seq
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array) return Object
   is (Arguments (2));

   function Evaluate_Leq
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is (To_Object
       (if To_Integer (Arguments (1)) <= To_Integer (Arguments (2))
          then 1 else 0));

   pragma Warnings (On);

   function Evaluate_Putchar
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array) return Object;

   function Evaluate_Trace
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array) return Object;

   ----------
   -- Bind --
   ----------

   procedure Bind (Name : String; Term : Skit.Terms.Term) is
   begin
      Skit.Handles.Bind
        (Handle, Name,
         Skit.Handles.Install
           (Handle, Skit.Compiler.Compile (Term),
            Resolve'Access));
   end Bind;

   ----------------------
   -- Evaluate_Putchar --
   ----------------------

   function Evaluate_Putchar
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is
      pragma Unreferenced (User_Data);
   begin
      Ada.Wide_Wide_Text_IO.Put
        (Wide_Wide_Character'Val (To_Integer (Arguments (3))));
      return To_Object (To_Integer (Arguments (1)) + 1);
   end Evaluate_Putchar;

   --------------------
   -- Evaluate_Trace --
   --------------------

   function Evaluate_Trace
     (User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is
      pragma Unreferenced (User_Data);
   begin
      Ada.Text_IO.Put_Line ("trace: " & Handle.Image (Arguments (1)));
      return Arguments (1);
   end Evaluate_Trace;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use all type Skit.Handles.Argument_Mode;
   begin
      Handle :=
        Skit.Handles.New_Handle
          (Core_Size => 16384,
           Writer => Ada.Text_IO.Put'Access);
      Handle.Bind ("#eq", Handle.Primitive (2, Evaluate_Eq'Access));
      Handle.Bind ("#leq", Handle.Primitive (2, Evaluate_Leq'Access));
      Handle.Bind ("#choose",
                   Handle.Primitive
                     ([Strict, Lazy, Lazy],
                      Evaluate_Choose'Access));
      Handle.Bind ("#seq",
                   Handle.Primitive
                     ([Strict, Lazy],
                      Evaluate_Seq'Access));
      Handle.Bind ("#add", Handle.Primitive (2, Evaluate_Add'Access));
      Handle.Bind ("#sub", Handle.Primitive (2, Evaluate_Sub'Access));
      Handle.Bind ("#mul", Handle.Primitive (2, Evaluate_Mul'Access));
      Handle.Bind ("#div", Handle.Primitive (2, Evaluate_Div'Access));
      Handle.Bind ("#mod", Handle.Primitive (2, Evaluate_Mod'Access));
      Handle.Bind ("#putchar", Handle.Primitive (3, Evaluate_Putchar'Access));
      Handle.Bind ("#trace", Handle.Primitive (1, Evaluate_Trace'Access));
      Handle.Bind ("#maxInt", To_Object (Max_Integer));
      Handle.Bind ("#minInt", To_Object (Min_Integer));

      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, "testlib.skit");
         while not End_Of_File (File) loop
            declare
               Expr   : constant String := Get_Line (File);
               Unused : constant Skit.Terms.Term :=
                          Skit.Parser.Parse (Expr, Bind'Access);
            begin
               pragma Unreferenced (Unused);
            end;
         end loop;
      end;
   end Initialize;

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

      Handle.Report;
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Fail));
   end Report;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Name : String)
      return Skit.Object
   is
      Value : constant Object := Handle.Lookup (Name);
   begin
      if Value = Undefined then
         raise Constraint_Error with
           "undefined: " & Name;
      else
         return Value;
      end if;
   end Resolve;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object)
   is
   begin
      Total := @ + 1;

      declare
         Term : Skit.Terms.Term := Load (Operations);
      begin
         Term := Skit.Compiler.Compile (Term);
         Handle.Install (Term, Resolve'Access);
      end;

      Handle.Evaluate;

      declare
         Result : constant Skit.Object := Handle.Pop;
      begin
         Put (Name, 38);

         if Result = Expected then
            Pass := @ + 1;
            Ada.Text_IO.Put_Line ("PASS");
         else
            Fail := @ + 1;
            Ada.Text_IO.Put ("expected: ");
            Handle.Write (Expected);
            Ada.Text_IO.Put ("; found: ");
            Handle.Write (Result);
            Ada.Text_IO.New_Line;
         end if;
      end;

   end Test;

   ----------
   -- Test --
   ----------

   procedure Test
     (Source   : String;
      Expected : Object)
   is
      Term : Skit.Terms.Term :=
               Skit.Parser.Parse (Source, Bind'Access);
   begin
      Total := @ + 1;

      Term := Skit.Compiler.Compile (Term);
      Handle.Install (Term, Resolve'Access);

      Handle.Evaluate;

      declare
         Result : constant Skit.Object := Handle.Pop;
      begin
         Put (Source, 38);
         Ada.Text_IO.Set_Col (40);

         if Result = Expected then
            Ada.Text_IO.Put_Line ("PASS");
            Pass := @ + 1;
         else
            Fail := @ + 1;
            Ada.Text_IO.Put ("expected: ");
            Handle.Write (Expected);
            Ada.Text_IO.Put ("; found: ");
            Handle.Write (Result);
            Ada.Text_IO.New_Line;
         end if;
      end;
   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("error evaluting " & Skit.Terms.Image (Term));
         raise;
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

   ----------
   -- Test --
   ----------

   procedure Test
     (Source   : String;
      Expected : String)
   is
      Term : Skit.Terms.Term :=
               Skit.Parser.Parse (Source, Bind'Access);
   begin
      Total := @ + 1;

      Term := Skit.Compiler.Compile (Term);
      Handle.Install (Term, Resolve'Access);

      Handle.Evaluate;

      declare
         Result : constant String :=
                    Handle.Image (Handle.Pop);
      begin
         Put (Source, 38);
         Ada.Text_IO.Set_Col (40);

         if Result = Expected then
            Ada.Text_IO.Put_Line ("PASS");
            Pass := @ + 1;
         else
            Fail := @ + 1;
            Ada.Text_IO.Put ("expected: ");
            Ada.Text_IO.Put (Expected);
            Ada.Text_IO.Put ("; found: ");
            Ada.Text_IO.Put (Result);
            Ada.Text_IO.New_Line;
            Fail := @ + 1;
         end if;
      end;

   end Test;

   ---------
   -- Var --
   ---------

   function Var (V : String) return Stack_Operation_Type is
   begin
      return Push (Skit.Terms.Symbol (V));
   end Var;

end Skit.Tests;
