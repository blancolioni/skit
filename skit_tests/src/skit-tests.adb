with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;

with Skit.Compiler;
with Skit.Handles;
with Skit.Parser;
with Skit.Terms;

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

   type Arithmetic_Function is (Add, Sub, Mul, Divide, Modulus, Leq);

   type Arithmetic_Evaluator (Fn : Arithmetic_Function) is
     new Primitive_Evaluator_Interface with
       record
         null;
       end record;

   overriding function Argument_Count
     (This : Arithmetic_Evaluator)
      return Natural
   is (2);

   overriding function Argument_Modes
     (This : Arithmetic_Evaluator)
      return Argument_Mode_Array
   is ([Strict, Strict]);

   overriding function Evaluate
     (This      : Arithmetic_Evaluator;
      User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object;

   type General_Evaluator_Fn is access
     function (Arguments : Object_Array) return Object;

   type Evaluator (Arg_Count : Natural) is
     new Primitive_Evaluator_Interface with
      record
         Modes : Argument_Mode_Array (1 .. Arg_Count);
         Fn    : General_Evaluator_Fn;
      end record;

   overriding function Argument_Count
     (This : Evaluator)
      return Natural
   is (This.Arg_Count);

   overriding function Argument_Modes
     (This : Evaluator)
      return Argument_Mode_Array
   is (This.Modes);

   overriding function Evaluate
     (This      : Evaluator;
      User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object;

   function Evaluate_Eq
     (Arguments : Object_Array)
      return Object
   is (To_Object
       (if Arguments (1) = Arguments (2)
          then 1 else 0));

   function Evaluate_Choose
     (Arguments : Object_Array) return Object
   is (if Arguments (1) = To_Object (0)
       then Arguments (2)
       else Arguments (3));

   function Evaluate_Seq
     (Arguments : Object_Array) return Object
   is (Arguments (2));

   function Evaluate_Putchar
     (Arguments : Object_Array) return Object;

   function Evaluate_Trace
     (Arguments : Object_Array) return Object;

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

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Arithmetic_Evaluator;
      User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is
      X : constant Integer := To_Integer (Arguments (1));
      Y : constant Integer := To_Integer (Arguments (2));
      Z : constant Integer :=
            (case This.Fn is
                when Add => X + Y,
                when Sub => X - Y,
                when Mul => X * Y,
                when Divide => X / Y,
                when Modulus => X mod Y,
                when Leq => (if X <= Y then 1 else 0));
   begin
      return To_Object (Z);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Evaluator;
      User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is
   begin
      return This.Fn (Arguments);
   end Evaluate;

   ----------------------
   -- Evaluate_Putchar --
   ----------------------

   function Evaluate_Putchar
     (Arguments : Object_Array)
      return Object
   is
   begin
      Ada.Wide_Wide_Text_IO.Put
        (Wide_Wide_Character'Val (To_Integer (Arguments (3))));
      return To_Object (To_Integer (Arguments (1)) + 1);
   end Evaluate_Putchar;

   --------------------
   -- Evaluate_Trace --
   --------------------

   function Evaluate_Trace
     (Arguments : Object_Array)
      return Object
   is
   begin
      Ada.Text_IO.Put_Line ("trace: " & Handle.Image (Arguments (1)));
      return Arguments (1);
   end Evaluate_Trace;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Handle :=
        Skit.Handles.New_Handle
          (Core_Size => 16384,
           Writer => Ada.Text_IO.Put'Access);
      Handle.Bind
        ("#eq",
         Handle.Primitive
           (Evaluator'(2, [Strict, Strict], Evaluate_Eq'Access)));

      Handle.Bind
        ("#leq", Handle.Primitive (Arithmetic_Evaluator'(Fn => Leq)));
      Handle.Bind ("#choose",
                   Handle.Primitive
                     (Evaluator'
                        (3, [Strict, Lazy, Lazy],
                         Evaluate_Choose'Access)));

      Handle.Bind ("#seq",
                   Handle.Primitive
                     (Evaluator'
                        (2, [Strict, Lazy],
                         Evaluate_Seq'Access)));

      Handle.Bind ("#add",
                   Handle.Primitive (Arithmetic_Evaluator'(Fn => Add)));
      Handle.Bind ("#sub",
                   Handle.Primitive (Arithmetic_Evaluator'(Fn => Sub)));
      Handle.Bind ("#mul",
                   Handle.Primitive (Arithmetic_Evaluator'(Fn => Mul)));
      Handle.Bind ("#div",
                   Handle.Primitive (Arithmetic_Evaluator'(Fn => Divide)));
      Handle.Bind ("#mod",
                   Handle.Primitive (Arithmetic_Evaluator'(Fn => Modulus)));

      Handle.Bind ("#putchar",
                   Handle.Primitive
                     (Evaluator'
                        (3, [Strict, Strict, Strict],
                         Evaluate_Putchar'Access)));

      Handle.Bind ("#trace",
                   Handle.Primitive
                     (Evaluator'
                        (1, [Strict],
                         Evaluate_Trace'Access)));

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
         Empty  : constant Boolean := Handle.Stack_Empty;
      begin
         Put (Source, 38);
         Ada.Text_IO.Set_Col (40);

         if Result = Expected and then Empty then
            Ada.Text_IO.Put_Line ("PASS");
            Pass := @ + 1;
         else
            Fail := @ + 1;
            if Result /= Expected then
               Ada.Text_IO.Put ("expected: ");
               Handle.Write (Expected);
               Ada.Text_IO.Put ("; found: ");
               Handle.Write (Result);
            end if;
            if not Empty then
               Ada.Text_IO.Put (" [stack not empty]");
            end if;
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
         Popped : constant Object := Handle.Pop;
         Empty  : constant Boolean := Handle.Stack_Empty;
         Result : constant String := Handle.Image (Popped);
      begin
         Put (Source, 38);
         Ada.Text_IO.Set_Col (40);

         if Result = Expected and then Empty then
            Ada.Text_IO.Put_Line ("PASS");
            Pass := @ + 1;
         else
            Fail := @ + 1;
            if Result /= Expected then
               Ada.Text_IO.Put ("expected: ");
               Ada.Text_IO.Put (Expected);
               Ada.Text_IO.Put ("; found: ");
               Ada.Text_IO.Put (Result);
            end if;
            if not Empty then
               Ada.Text_IO.Put (" [stack not empty]");
            end if;
            Ada.Text_IO.New_Line;
         end if;
      end;

   end Test;

end Skit.Tests;
