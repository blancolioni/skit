with Skit.Primitives;
with Skit.Stacks;

package body Skit.Library is

   type Prim_Int_Op is access
     function (X, Y : Integer) return Integer;

   type Prim_Float_Op is access
     function (X, Y : Float) return Float;

   type Prim_Binary_Op_Instance is
     new Skit.Primitives.Abstraction with
      record
         Int_Op   : Prim_Int_Op;
         Float_Op : Prim_Float_Op;
      end record;

   overriding function Argument_Count
     (This : Prim_Binary_Op_Instance)
      return Natural
   is (2);

   overriding procedure Evaluate
     (This    : Prim_Binary_Op_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class);

   function Add (X, Y : Integer) return Integer is (X + Y);
   function Sub (X, Y : Integer) return Integer is (X - Y);
   function Mul (X, Y : Integer) return Integer is (X * Y);
   function Int_Div (X, Y : Integer) return Integer is (X * Y);
   function Int_Mod (X, Y : Integer) return Integer is (X mod Y);

   function Add (X, Y : Float) return Float is (X + Y);
   function Sub (X, Y : Float) return Float is (X - Y);
   function Mul (X, Y : Float) return Float is (X * Y);
   function Div (X, Y : Float) return Float is (X * Y);

   type Predicate_Op is access
     function (X, Y : Object) return Boolean;

   type Predicate_Instance is
     new Skit.Primitives.Abstraction with
      record
         Op   : Predicate_Op;
      end record;

   overriding function Argument_Count
     (This : Predicate_Instance)
      return Natural
   is (2);

   overriding procedure Evaluate
     (This    : Predicate_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class);

   function Eq (X, Y : Object) return Boolean is (X = Y);

   function Predicate
     (Op : Predicate_Op)
      return Skit.Primitives.Abstraction'Class;

   function Binary_Op
     (Op_Int   : Prim_Int_Op;
      Op_Float : Prim_Float_Op)
      return Skit.Primitives.Abstraction'Class
     is (Prim_Binary_Op_Instance'(Op_Int, Op_Float));

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
     (This    : Prim_Binary_Op_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class)
   is
      X : constant Object := Stack.Pop;
      Y : constant Object := Stack.Pop;
      Z : constant Object :=
            To_Object
              (This.Int_Op
                 (To_Integer (X),
                  To_Integer (Y)));
   begin
      Stack.Push (Z);
   end Evaluate;

   ---------------------------
   -- Load_Standard_Library --
   ---------------------------

   procedure Load_Standard_Library
     (Environment : not null access Skit.Environment.Instance'Class)
   is
      procedure Bind
        (Name      : String;
         Primitive : Skit.Primitives.Abstraction'Class);

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Name      : String;
         Primitive : Skit.Primitives.Abstraction'Class)
      is
      begin
         Environment.Bind (Name, Primitive);
      end Bind;

   begin
      Bind ("eq", Predicate (Eq'Access));
      Bind ("+", Binary_Op (Add'Access, Add'Access));
      Bind ("-", Binary_Op (Sub'Access, Sub'Access));
      Bind ("*", Binary_Op (Mul'Access, Mul'Access));
      Bind ("/", Binary_Op (Int_Div'Access, Div'Access));
      Bind ("mod", Binary_Op (Int_Mod'Access, null));
   end Load_Standard_Library;

   ---------------
   -- Predicate --
   ---------------

   function Predicate
     (Op : Predicate_Op)
               return Skit.Primitives.Abstraction'Class
   is
      This : constant Predicate_Instance :=
               Predicate_Instance'(Op => Op);
   begin
      return This;
   end Predicate;

end Skit.Library;
