with Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Streams;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;

with Ada.Strings.Unbounded;

with Skit.Compiler;
with Skit.Handles;
with Skit.Handles.Images;
with Skit.Parser;

package body Skit.Tests is

   --  A concrete foreign object for the GC test.  It carries N Object children
   --  (relocated on collection via Visit) and records, in the Freed observer
   --  below, that Free ran -- so the test can tell survivors from swept
   --  objects without dereferencing a reclaimed one.

   Freed : array (1 .. 8) of Boolean := [others => False];

   type Box (N : Natural) is new Foreign_Object_Interface with
      record
         Id       : Positive;
         Children : Object_Array (1 .. N);
      end record;

   overriding function Class_Name (This : Box) return String;

   overriding function Serialize (This : Box)
      return Ada.Streams.Stream_Element_Array;

   overriding procedure Visit
     (This    : in out Box;
      Process : not null access procedure (Child : in out Object));

   overriding procedure Free (This : in out Box);

   overriding function Image (This : Box) return String;

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
         Empty  : constant Boolean := Handle.Stack_Empty;
      begin
         Put (Name, 38);

         if Result = Expected and then Empty then
            Pass := @ + 1;
            Ada.Text_IO.Put_Line ("PASS");
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

   ----------------
   -- Class_Name --
   ----------------

   overriding function Class_Name (This : Box) return String is
      pragma Unreferenced (This);
   begin
      return "box";
   end Class_Name;

   ---------------
   -- Serialize --
   ---------------

   overriding function Serialize (This : Box)
      return Ada.Streams.Stream_Element_Array
   is
      pragma Unreferenced (This);
      Empty : Ada.Streams.Stream_Element_Array (1 .. 0);
   begin
      return Empty;
   end Serialize;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This    : in out Box;
      Process : not null access procedure (Child : in out Object))
   is
   begin
      for I in This.Children'Range loop
         Process (This.Children (I));
      end loop;
   end Visit;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Box) is
   begin
      Freed (This.Id) := True;
   end Free;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Box) return String is
      pragma Unreferenced (This);
   begin
      return "<box>";
   end Image;

   ---------------------------
   -- Test_Foreign_Objects --
   ---------------------------

   procedure Test_Foreign_Objects is

      H : constant Skit.Handles.Handle :=
            Skit.Handles.New_Handle (Core_Size => 1024);

      function No_Resolve (Name : String) return Object;

      procedure Check (Name : String; Cond : Boolean);

      ----------------
      -- No_Resolve --
      ----------------

      function No_Resolve (Name : String) return Object is
         pragma Unreferenced (Name);
      begin
         return Undefined;
      end No_Resolve;

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Cond : Boolean) is
      begin
         Total := @ + 1;
         Put (Name, 38);
         Ada.Text_IO.Set_Col (40);
         if Cond then
            Pass := @ + 1;
            Ada.Text_IO.Put_Line ("PASS");
         else
            Fail := @ + 1;
            Ada.Text_IO.Put_Line ("FAIL");
         end if;
      end Check;

      function Cell (Left, Right : Skit.Terms.Term) return Object;

      ----------
      -- Cell --
      ----------

      function Cell (Left, Right : Skit.Terms.Term) return Object is
      begin
         return H.Install
           (Skit.Compiler.Compile (Skit.Terms.Apply (Left, Right)),
            No_Resolve'Access);
      end Cell;

      --  Child of box 1: an App (42, 43) cell reachable only through the box,
      --  so it survives a collection only if Visit forwards it.
      Cc   : constant Object :=
               Cell (Skit.Terms.Const (42), Skit.Terms.Const (43));

      B1 : constant Foreign_Reference :=
             new Box'(N => 1, Id => 1, Children => [Cc]);
      B2 : constant Foreign_Reference :=
             new Box'(N => 0, Id => 2, Children => []);
      B3 : constant Foreign_Reference :=
             new Box'(N => 0, Id => 3, Children => []);

      Obj1 : constant Object := H.Bind_Object (B1);
      Obj2 : constant Object := H.Bind_Object (B2);
      Obj3 : constant Object := H.Bind_Object (B3);
   begin
      --  Keep box 1 alive through a live cell (found by live-cell discovery)
      --  and box 3 through a bare environment root (found by root marking).
      --  Box 2 is left unreferenced; it must be swept.
      H.Bind
        ("keep1",
         Cell (Skit.Terms.Primitive (Obj1), Skit.Terms.Const (0)));
      H.Bind ("keep3", Obj3);

      H.Unpin (Obj1);
      H.Unpin (Obj2);
      H.Unpin (Obj3);

      --  Seed the stack, then churn allocations to force several collections.
      H.Install (Skit.Compiler.Compile (Skit.Terms.Const (0)),
                 No_Resolve'Access);
      for I in 1 .. 4000 loop
         H.Push (To_Object (I));
         declare
            Discard : constant Object := H.Pop;
            pragma Unreferenced (Discard);
         begin
            null;
         end;
      end loop;

      Check ("foreign: unreachable box swept", Freed (2));
      Check ("foreign: box kept via live cell", not Freed (1));
      Check ("foreign: box kept via bare root", not Freed (3));

      declare
         Child : constant Object := Box (B1.all).Children (1);
      begin
         Check
           ("foreign: child forwarded",
            Is_Application (Child)
            and then H.Left (Child) = To_Object (42)
            and then H.Right (Child) = To_Object (43));
      end;

      declare
         B4   : constant Foreign_Reference :=
                  new Box'(N => 0, Id => 4, Children => []);
         Obj4 : constant Object := H.Bind_Object (B4);
      begin
         Check ("foreign: swept slot reused", Obj4 = Obj2);
      end;

      H.Free_Foreign_Objects;
      Check ("foreign: shutdown frees survivors",
             Freed (1) and then Freed (3) and then Freed (4));
   end Test_Foreign_Objects;

   -----------------
   -- Test_Images --
   -----------------

   procedure Test_Images is
      use Ada.Strings.Unbounded;

      Path : constant String := "test_image.skix";

      Hw : constant Skit.Handles.Handle :=
             Skit.Handles.New_Handle (Core_Size => 1024);
      Hr : constant Skit.Handles.Handle :=
             Skit.Handles.New_Handle (Core_Size => 1024);

      function No_Resolve (Name : String) return Object;

      procedure Check (Name : String; Cond : Boolean);

      ----------------
      -- No_Resolve --
      ----------------

      function No_Resolve (Name : String) return Object is
         pragma Unreferenced (Name);
      begin
         return Undefined;
      end No_Resolve;

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Cond : Boolean) is
      begin
         Total := @ + 1;
         Put (Name, 38);
         Ada.Text_IO.Set_Col (40);
         if Cond then
            Pass := @ + 1;
            Ada.Text_IO.Put_Line ("PASS");
         else
            Fail := @ + 1;
            Ada.Text_IO.Put_Line ("FAIL");
         end if;
      end Check;

      --  The graph K 42 99 == App (App (K, 42), 99): two nested cells, an
      --  integer at each level and a combinator at the bottom.
      Graph : constant Skit.Terms.Term :=
                Skit.Terms.Apply
                  (Skit.Terms.Apply
                     (Skit.Terms.Combinator (Skit.K),
                      Skit.Terms.Const (42)),
                   Skit.Terms.Const (99));

      Root : constant Object :=
               Hw.Install (Skit.Compiler.Compile (Graph),
                           No_Resolve'Access);
   begin
      Hw.Bind ("root", Root);
      Skit.Handles.Images.Write
        (Hw, Path, [1 => To_Unbounded_String ("root")]);

      Skit.Handles.Images.Read (Hr, Path);

      declare
         RB    : constant Object := Hr.Lookup ("root");
         Inner : constant Object :=
                   (if Is_Application (RB) then Hr.Left (RB) else Undefined);
      begin
         Check ("image: export is an application", Is_Application (RB));
         Check ("image: outer right leaf preserved",
                Is_Application (RB) and then Hr.Right (RB) = To_Object (99));
         Check ("image: inner node is an application",
                Is_Application (Inner));
         Check ("image: combinator preserved",
                Is_Application (Inner) and then Hr.Left (Inner) = Skit.K);
         Check ("image: inner int leaf preserved",
                Is_Application (Inner)
                and then Hr.Right (Inner) = To_Object (42));
      end;

      if Ada.Directories.Exists (Path) then
         Ada.Directories.Delete_File (Path);
      end if;
   end Test_Images;

   ---------
   -- Var --
   ---------

   function Var (V : String) return Stack_Operation_Type is
   begin
      return Push (Skit.Terms.Symbol (V));
   end Var;

end Skit.Tests;
