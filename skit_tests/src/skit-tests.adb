with Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;
with Interfaces;

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

   --  Observers for the image round-trip: what the class factory last rebuilt.
   Last_Des_Id          : Natural := 0;
   Last_Des_Child_Count : Natural := 0;
   Last_Des_Child       : Object  := Undefined;

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

   --  A class factory: rebuild a Box from its serialized Id and its (already
   --  relocated) children, recording what it saw in the observers above.
   function Box_Deserialize
     (Bytes    : Ada.Streams.Stream_Element_Array;
      Children : Object_Array)
      return Foreign_Reference;

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
      use Ada.Streams;
      use Interfaces;
      R : Stream_Element_Array (1 .. 4);
      V : Unsigned_32 := Unsigned_32 (This.Id);
   begin
      for I in R'Range loop
         R (I) := Stream_Element (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
      return R;
   end Serialize;

   ---------------------
   -- Box_Deserialize --
   ---------------------

   function Box_Deserialize
     (Bytes    : Ada.Streams.Stream_Element_Array;
      Children : Object_Array)
      return Foreign_Reference
   is
      use Interfaces;
      V : Unsigned_32 := 0;
   begin
      for I in reverse Bytes'Range loop
         V := Shift_Left (V, 8) or Unsigned_32 (Bytes (I));
      end loop;
      Last_Des_Id          := Natural (V);
      Last_Des_Child_Count := Children'Length;
      if Children'Length >= 1 then
         Last_Des_Child := Children (Children'First);
      end if;
      return new Box'(N        => Children'Length,
                      Id       => Positive (V),
                      Children => Children);
   end Box_Deserialize;

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

   -------------------------
   -- Test_Foreign_Nested --
   -------------------------

   procedure Test_Foreign_Nested is

      function No_Resolve (Name : String) return Object;

      procedure Check (Name : String; Cond : Boolean);

      procedure Churn (H : Skit.Handles.Handle);

      H : constant Skit.Handles.Handle :=
            Skit.Handles.New_Handle (Core_Size => 1024);

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

      -----------
      -- Churn --
      -----------

      --  Allocate and discard until several collections have run.
      procedure Churn (H : Skit.Handles.Handle) is
      begin
         H.Install
           (Skit.Compiler.Compile (Skit.Terms.Const (0)), No_Resolve'Access);
         for I in 1 .. 4000 loop
            H.Push (To_Object (I));
            declare
               Discard : constant Object := H.Pop;
               pragma Unreferenced (Discard);
            begin
               null;
            end;
         end loop;
      end Churn;

      --  Inner box, reachable only through the outer box's child cell.
      Inner    : constant Foreign_Reference :=
                   new Box'(N => 0, Id => 5, Children => []);
      Obj_Inner : constant Object := H.Bind_Object (Inner);

      --  A cell App (Obj_Inner, 0) held as the outer box's child.
      Child : constant Object :=
                H.Install
                  (Skit.Compiler.Compile
                     (Skit.Terms.Apply
                        (Skit.Terms.Primitive (Obj_Inner),
                         Skit.Terms.Const (0))),
                   No_Resolve'Access);

      Outer     : constant Foreign_Reference :=
                    new Box'(N => 1, Id => 6, Children => [Child]);
      Obj_Outer : constant Object := H.Bind_Object (Outer);
   begin
      --  Keep the outer box via a bare root; the inner box is reachable only
      --  through the outer box's child cell, so only the discovery fixpoint
      --  (a later round, after the child is forwarded) can find it.
      H.Bind ("outer", Obj_Outer);
      H.Unpin (Obj_Outer);
      H.Unpin (Obj_Inner);

      Churn (H);
      Check ("foreign nested: outer survives", not Freed (6));
      Check ("foreign nested: inner survives via nesting", not Freed (5));

      --  Drop the outer box; both must now be collected.
      H.Bind ("outer", To_Object (0));
      Churn (H);
      Check ("foreign nested: outer collected when dropped", Freed (6));
      Check ("foreign nested: inner collected transitively", Freed (5));
   end Test_Foreign_Nested;

   -----------------
   -- Test_Images --
   -----------------

   procedure Test_Images is
      use Ada.Strings.Unbounded;
      package Img renames Skit.Handles.Images;
      package T renames Skit.Terms;

      Path : constant String := "test_image.skix";

      function No_Resolve (Name : String) return Object;

      procedure Check (Name : String; Cond : Boolean);

      function New_Machine return Skit.Handles.Handle
      is (Skit.Handles.New_Handle (Core_Size => 1024));

      function U (S : String) return Unbounded_String
        renames To_Unbounded_String;

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

   begin
      --  Structure: K 42 99 == App (App (K, 42), 99).  Two nested cells, an
      --  integer at each level and a combinator at the bottom.
      declare
         Hw   : constant Skit.Handles.Handle := New_Machine;
         Hr   : constant Skit.Handles.Handle := New_Machine;
         Root : constant Object :=
                  Hw.Install
                    (Skit.Compiler.Compile
                       (T.Apply
                          (T.Apply (T.Combinator (Skit.K), T.Const (42)),
                           T.Const (99))),
                     No_Resolve'Access);
      begin
         Hw.Bind ("root", Root);
         Img.Write (Hw, Path, [1 => U ("root")]);
         Img.Read (Hr, Path);
         declare
            RB    : constant Object := Hr.Lookup ("root");
            Inner : constant Object :=
                      (if Is_Application (RB) then Hr.Left (RB)
                       else Undefined);
         begin
            Check ("image: export is an application", Is_Application (RB));
            Check ("image: outer right leaf preserved",
                   Is_Application (RB)
                   and then Hr.Right (RB) = To_Object (99));
            Check ("image: inner node is an application",
                   Is_Application (Inner));
            Check ("image: combinator preserved",
                   Is_Application (Inner) and then Hr.Left (Inner) = Skit.K);
            Check ("image: inner int leaf preserved",
                   Is_Application (Inner)
                   and then Hr.Right (Inner) = To_Object (42));
         end;
      end;

      --  Semantic round-trip: S K K 42 reduces to 42 after a reload into a
      --  fresh machine.
      declare
         Hw : constant Skit.Handles.Handle := New_Machine;
         Hr : constant Skit.Handles.Handle := New_Machine;

         function From_Reader (Name : String) return Object
         is (Hr.Lookup (Name));

         Root : constant Object :=
                  Hw.Install
                    (Skit.Compiler.Compile
                       (T.Apply
                          (T.Apply
                             (T.Apply (T.Combinator (Skit.S),
                                       T.Combinator (Skit.K)),
                              T.Combinator (Skit.K)),
                           T.Const (42))),
                     No_Resolve'Access);
      begin
         Hw.Bind ("f", Root);
         Img.Write (Hw, Path, [1 => U ("f")]);
         Img.Read (Hr, Path);
         Hr.Install
           (Skit.Compiler.Compile (T.Symbol ("f")), From_Reader'Access);
         Hr.Evaluate;
         Check ("image: evaluates to same value after round-trip",
                Hr.Pop = To_Object (42));
      end;

      --  Cyclic graph: Y K evaluates to a single self-referential cell
      --  App (K, self); the writer must break the cycle and the reader must
      --  re-tie the self reference.
      declare
         Hw : constant Skit.Handles.Handle := New_Machine;
         Hr : constant Skit.Handles.Handle := New_Machine;
      begin
         Hw.Install
           (Skit.Compiler.Compile
              (T.Apply (T.Combinator (Skit.Y), T.Combinator (Skit.K))),
            No_Resolve'Access);
         Hw.Evaluate;
         Hw.Bind ("cyc", Hw.Pop);
         Img.Write (Hw, Path, [1 => U ("cyc")]);
         Img.Read (Hr, Path);
         declare
            --  Y K evaluates to  W = App (K, X),  X = App (K, X):  X is a
            --  self-referential cell (Right (X) = X).  The writer must break
            --  that self-loop and the reader must re-tie it, so following
            --  Right into X and again stays at X.
            CB : constant Object := Hr.Lookup ("cyc");
            X  : constant Object :=
                   (if Is_Application (CB) then Hr.Right (CB) else Undefined);
         begin
            Check ("image: self-referential cell preserved",
                   Is_Application (CB)
                   and then Hr.Left (CB) = Skit.K
                   and then Is_Application (X)
                   and then Hr.Left (X) = Skit.K
                   and then Hr.Right (X) = X);
         end;
      end;

      --  Several exports, including bare (non-application) immediates.
      declare
         Hw : constant Skit.Handles.Handle := New_Machine;
         Hr : constant Skit.Handles.Handle := New_Machine;
      begin
         Hw.Bind ("n", To_Object (7));
         Hw.Bind ("c", Skit.K);
         Img.Write (Hw, Path, [U ("n"), U ("c")]);
         Img.Read (Hr, Path);
         Check ("image: bare integer export", Hr.Lookup ("n") = To_Object (7));
         Check ("image: bare combinator export", Hr.Lookup ("c") = Skit.K);
      end;

      --  Negative integer and float leaves.
      declare
         Hw   : constant Skit.Handles.Handle := New_Machine;
         Hr   : constant Skit.Handles.Handle := New_Machine;
         Root : constant Object :=
                  Hw.Install
                    (Skit.Compiler.Compile
                       (T.Apply
                          (T.Apply (T.Combinator (Skit.K), T.Const (-5)),
                           T.Const (Long_Float'(0.5)))),
                     No_Resolve'Access);
      begin
         Hw.Bind ("g", Root);
         Img.Write (Hw, Path, [1 => U ("g")]);
         Img.Read (Hr, Path);
         declare
            GB : constant Object := Hr.Lookup ("g");
         begin
            Check ("image: negative int leaf",
                   Is_Application (GB)
                   and then Is_Application (Hr.Left (GB))
                   and then Hr.Right (Hr.Left (GB)) = To_Object (-5));
            Check ("image: float leaf",
                   Is_Application (GB)
                   and then Hr.Right (GB) = To_Object (Long_Float'(0.5)));
         end;
      end;

      --  Errors: an unknown export, and an object the MVP cannot serialize.
      declare
         Hw     : constant Skit.Handles.Handle := New_Machine;
         Caught : Boolean := False;
      begin
         begin
            Img.Write (Hw, Path, [1 => U ("does-not-exist")]);
         exception
            when Img.Image_Error => Caught := True;
         end;
         Check ("image: unknown export rejected", Caught);
      end;

      declare
         Hw     : constant Skit.Handles.Handle := New_Machine;
         Caught : Boolean := False;
      begin
         Hw.Bind ("p", Hw.Primitive (Arithmetic_Evaluator'(Fn => Add)));
         begin
            Img.Write (Hw, Path, [1 => U ("p")]);
         exception
            when Img.Image_Error => Caught := True;
         end;
         Check ("image: bare primitive export rejected", Caught);
      end;

      --  Named imports: a graph referencing the primitive #add serializes with
      --  the primitive as a by-name import, and re-links to a *different*
      --  machine's #add on load (the build-specific opcode never crosses).
      declare
         Hw   : constant Skit.Handles.Handle := New_Machine;
         Hr   : constant Skit.Handles.Handle := New_Machine;
         Hbad : constant Skit.Handles.Handle := New_Machine;

         function From_Writer (Name : String) return Object
         is (Hw.Lookup (Name));
         function From_Reader (Name : String) return Object
         is (Hr.Lookup (Name));

         Caught : Boolean := False;
      begin
         Hw.Bind ("#add", Hw.Primitive (Arithmetic_Evaluator'(Fn => Add)));
         Hw.Bind
           ("sum",
            Hw.Install
              (Skit.Compiler.Compile
                 (T.Apply
                    (T.Apply (T.Symbol ("#add"), T.Const (2)),
                     T.Const (3))),
               From_Writer'Access));
         Img.Write (Hw, Path, [1 => U ("sum")]);

         --  Reader that provides its own #add: the import must resolve to it.
         Hr.Bind ("#add", Hr.Primitive (Arithmetic_Evaluator'(Fn => Add)));
         Img.Read (Hr, Path);
         Hr.Install
           (Skit.Compiler.Compile (T.Symbol ("sum")), From_Reader'Access);
         Hr.Evaluate;
         Check ("image: named import re-links and evaluates",
                Hr.Pop = To_Object (5));

         --  Reader lacking #add: the import cannot resolve.
         begin
            Img.Read (Hbad, Path);
         exception
            when Img.Image_Error => Caught := True;
         end;
         Check ("image: unresolved import rejected", Caught);
      end;

      --  Symbol atoms: a graph carrying an (unresolved) symbol re-interns the
      --  symbol by name into the reader -- its object is the reader's own
      --  symbol for that name, not the writer's.
      declare
         Hw : constant Skit.Handles.Handle := New_Machine;
         Hr : constant Skit.Handles.Handle := New_Machine;

         --  Resolve leaves references as symbols rather than values.
         function As_Symbol (Name : String) return Object
         is (Hw.Intern_Symbol (Name));

         Root : constant Object :=
                  Hw.Install
                    (Skit.Compiler.Compile
                       (T.Apply (T.Symbol ("foo"), T.Const (1))),
                     As_Symbol'Access);
      begin
         Hw.Bind ("g", Root);
         Img.Write (Hw, Path, [1 => U ("g")]);
         Img.Read (Hr, Path);
         declare
            GB : constant Object := Hr.Lookup ("g");
         begin
            Check ("image: symbol re-interned by name",
                   Is_Application (GB)
                   and then Is_Symbol (Hr.Left (GB))
                   and then Hr.Left (GB) = Hr.Intern_Symbol ("foo")
                   and then Hr.Right (GB) = To_Object (1));
         end;
      end;

      --  Foreign objects: a Box (carrying an Id and a child cell) serializes
      --  via its class + bytes + child vector, and is rebuilt on load by the
      --  factory registered for its class.
      declare
         Hw   : constant Skit.Handles.Handle := New_Machine;
         Hr   : constant Skit.Handles.Handle := New_Machine;
         Hbad : constant Skit.Handles.Handle := New_Machine;

         Child : constant Object :=
                   Hw.Install
                     (Skit.Compiler.Compile
                        (T.Apply (T.Combinator (Skit.K), T.Const (5))),
                      No_Resolve'Access);
         Bw    : constant Foreign_Reference :=
                   new Box'(N => 1, Id => 77, Children => [Child]);
         Obj_B : constant Object := Hw.Bind_Object (Bw);

         Caught : Boolean := False;
      begin
         Hw.Bind ("b", Obj_B);
         Img.Write (Hw, Path, [1 => U ("b")]);

         Hr.Register_Object_Class ("box", Box_Deserialize'Access);
         Last_Des_Id          := 0;
         Last_Des_Child_Count := 0;
         Last_Des_Child       := Undefined;
         Img.Read (Hr, Path);

         Check ("image: foreign object rebuilt",
                Is_Foreign_Object (Hr.Lookup ("b")));
         Check ("image: foreign bytes round-trip", Last_Des_Id = 77);
         Check ("image: foreign child count", Last_Des_Child_Count = 1);
         Check ("image: foreign child relocated",
                Is_Application (Last_Des_Child)
                and then Hr.Left (Last_Des_Child) = Skit.K
                and then Hr.Right (Last_Des_Child) = To_Object (5));

         --  Reader without the class factory: cannot rebuild.
         begin
            Img.Read (Hbad, Path);
         exception
            when Img.Image_Error => Caught := True;
         end;
         Check ("image: unregistered foreign class rejected", Caught);
      end;

      --  Checksum: a single flipped byte in the body is detected on load.
      declare
         Hw     : constant Skit.Handles.Handle := New_Machine;
         Hr     : constant Skit.Handles.Handle := New_Machine;
         Caught : Boolean := False;
      begin
         Hw.Bind ("n", To_Object (7));
         Img.Write (Hw, Path, [1 => U ("n")]);
         declare
            use type Ada.Streams.Stream_Element;
            use type Ada.Streams.Stream_Element_Offset;
            package SIO renames Ada.Streams.Stream_IO;
            F : SIO.File_Type;
         begin
            SIO.Open (F, SIO.In_File, Path);
            declare
               Len  : constant SIO.Count := SIO.Size (F);
               D    : Ada.Streams.Stream_Element_Array
                        (1 .. Ada.Streams.Stream_Element_Offset (Len));
               Last : Ada.Streams.Stream_Element_Offset;
               Mid  : constant Ada.Streams.Stream_Element_Offset :=
                        1 + D'Length / 2;
            begin
               SIO.Read (F, D, Last);
               SIO.Close (F);
               D (Mid) := D (Mid) xor 16#FF#;
               SIO.Create (F, SIO.Out_File, Path);
               SIO.Write (F, D);
               SIO.Close (F);
            end;
         end;
         begin
            Img.Read (Hr, Path);
         exception
            when Img.Image_Error => Caught := True;
         end;
         Check ("image: corrupted image rejected by checksum", Caught);
      end;

      --  Fingerprint: over export names.  Same names -> same fingerprint even
      --  with different contents; different names -> different fingerprint.
      declare
         use type Interfaces.Unsigned_32;

         H1 : constant Skit.Handles.Handle := New_Machine;
         H2 : constant Skit.Handles.Handle := New_Machine;
         H3 : constant Skit.Handles.Handle := New_Machine;
         Fp_AB, Fp_AB2, Fp_AC : Interfaces.Unsigned_32;
      begin
         H1.Bind ("a", To_Object (1));
         H1.Bind ("b", To_Object (2));
         Img.Write (H1, Path, [U ("a"), U ("b")]);
         Fp_AB := Img.Fingerprint (Path);

         H2.Bind ("a", To_Object (99));   --  same names, different values
         H2.Bind ("b", To_Object (100));
         Img.Write (H2, Path, [U ("a"), U ("b")]);
         Fp_AB2 := Img.Fingerprint (Path);

         H3.Bind ("a", To_Object (1));
         H3.Bind ("c", To_Object (2));    --  different export name
         Img.Write (H3, Path, [U ("a"), U ("c")]);
         Fp_AC := Img.Fingerprint (Path);

         Check ("image: fingerprint stable across contents", Fp_AB = Fp_AB2);
         Check ("image: fingerprint changes with exports", Fp_AB /= Fp_AC);
      end;

      --  Sibling resolution: an import prefers a co-loaded module's export
      --  over the standing environment.
      declare
         Ha : constant Skit.Handles.Handle := New_Machine;
         Hb : constant Skit.Handles.Handle := New_Machine;
         Hr : constant Skit.Handles.Handle := New_Machine;
         Path_A : constant String := "test_a.skix";
         Path_B : constant String := "test_b.skix";

         function From_A (Name : String) return Object is (Ha.Lookup (Name));
      begin
         --  Module A imports "shared"; module B exports "shared" as K 42.
         Ha.Bind ("shared", Ha.Primitive (Arithmetic_Evaluator'(Fn => Add)));
         Ha.Bind
           ("use",
            Ha.Install
              (Skit.Compiler.Compile
                 (T.Apply (T.Symbol ("shared"), T.Const (7))),
               From_A'Access));
         Img.Write (Ha, Path_A, [1 => U ("use")]);

         Hb.Bind
           ("shared",
            Hb.Install
              (Skit.Compiler.Compile
                 (T.Apply (T.Combinator (Skit.K), T.Const (42))),
               From_A'Access));
         Img.Write (Hb, Path_B, [1 => U ("shared")]);

         --  The environment already binds "shared" -- the sibling must win.
         Hr.Bind ("shared", To_Object (999));
         Img.Read (Hr, Img.Name_Array'[U (Path_A), U (Path_B)]);

         declare
            Use_G  : constant Object := Hr.Lookup ("use");
            Shared : constant Object := Hr.Lookup ("shared");
         begin
            Check ("sibling: import bound to sibling export",
                   Is_Application (Use_G)
                   and then Hr.Left (Use_G) = Shared);
            Check ("sibling: sibling export wins over environment",
                   Is_Application (Shared)
                   and then Hr.Left (Shared) = Skit.K
                   and then Hr.Right (Shared) = To_Object (42));
         end;

         if Ada.Directories.Exists (Path_A) then
            Ada.Directories.Delete_File (Path_A);
         end if;
         if Ada.Directories.Exists (Path_B) then
            Ada.Directories.Delete_File (Path_B);
         end if;
      end;

      --  Mutual references: A imports B, B imports A.  Only the two-pass load
      --  can link them -- every export is registered before any import.
      declare
         Ha : constant Skit.Handles.Handle := New_Machine;
         Hb : constant Skit.Handles.Handle := New_Machine;
         Hr : constant Skit.Handles.Handle := New_Machine;
         Path_A : constant String := "test_a.skix";
         Path_B : constant String := "test_b.skix";

         function From_A (Name : String) return Object is (Ha.Lookup (Name));
         function From_B (Name : String) return Object is (Hb.Lookup (Name));
      begin
         Ha.Bind ("b", Ha.Primitive (Arithmetic_Evaluator'(Fn => Add)));
         Ha.Bind
           ("a",
            Ha.Install
              (Skit.Compiler.Compile
                 (T.Apply (T.Symbol ("b"), T.Const (1))),
               From_A'Access));
         Img.Write (Ha, Path_A, [1 => U ("a")]);

         Hb.Bind ("a", Hb.Primitive (Arithmetic_Evaluator'(Fn => Add)));
         Hb.Bind
           ("b",
            Hb.Install
              (Skit.Compiler.Compile
                 (T.Apply (T.Symbol ("a"), T.Const (2))),
               From_B'Access));
         Img.Write (Hb, Path_B, [1 => U ("b")]);

         Img.Read (Hr, Img.Name_Array'[U (Path_A), U (Path_B)]);

         declare
            Ga : constant Object := Hr.Lookup ("a");
            Gb : constant Object := Hr.Lookup ("b");
         begin
            Check ("sibling: mutual import a -> b",
                   Is_Application (Ga)
                   and then Hr.Left (Ga) = Gb
                   and then Hr.Right (Ga) = To_Object (1));
            Check ("sibling: mutual import b -> a",
                   Is_Application (Gb)
                   and then Hr.Left (Gb) = Ga
                   and then Hr.Right (Gb) = To_Object (2));
         end;

         if Ada.Directories.Exists (Path_A) then
            Ada.Directories.Delete_File (Path_A);
         end if;
         if Ada.Directories.Exists (Path_B) then
            Ada.Directories.Delete_File (Path_B);
         end if;
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
