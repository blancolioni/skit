private with System.Storage_Elements;
private with System.Storage_Pools;

package Skit.Terms is

   type Term is private;

   function Apply (Left, Right : Term) return Term;
   function Lambda (Name : String; Expr : Term) return Term;
   function Combinator (C : Object) return Term;
   function Const (Value : Integer) return Term;
   function Const (Value : Long_Float) return Term;
   function Symbol (Name : String) return Term;
   function Primitive (P : Object) return Term;

   function Is_Application (T : Term) return Boolean;
   function Is_Lambda (T : Term) return Boolean;
   function Is_Symbol (T : Term) return Boolean;
   function Is_Atom (T : Term) return Boolean;

   function Is_Combinator (T : Term; C : Object) return Boolean;

   function Get_Left (T : Term) return Term
     with Pre => Is_Application (T);

   function Get_Right (T : Term) return Term
     with Pre => Is_Application (T);

   function Get_Variable (T : Term) return String
     with Pre => Is_Lambda (T);

   function Get_Body (T : Term) return Term
     with Pre => Is_Lambda (T);

   function Get_Symbol (T : Term) return String
     with Pre => Is_Symbol (T);

   function Get_Atom (T : Term) return Object
     with Pre => Is_Atom (T);

   procedure Reset;

   function Image (T : Term) return String;

private

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   Max_Blocks : constant := 100;

   type Memory_Array is array (1 .. Max_Blocks) of Storage_Array_Access;

   type Term_Pool is new System.Storage_Pools.Root_Storage_Pool with
      record
         Memory      : Memory_Array;
         Top         : System.Storage_Elements.Storage_Offset := 0;
         Next        : System.Storage_Elements.Storage_Offset := 0;
         Start       : System.Storage_Elements.Storage_Offset := 0;
         Block_Index : Natural := 0;
      end record;

   overriding procedure Allocate
     (Pool                     : in out Term_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool                     : in out Term_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding function Storage_Size
     (Pool : Term_Pool)
      return System.Storage_Elements.Storage_Count;

   Arena : Term_Pool;

   type Term_Record;
   type Term is access Term_Record;
   for Term'Storage_Pool use Arena;

   type Term_Class is (Apply, Lambda,
                       Const_Integer, Const_Float, Primitive, Symbol);

   subtype Atom_Class is Term_Class range Const_Integer .. Primitive;

   type Term_Record (Class : Term_Class; Length : Natural) is
      record
         case Class is
            when Apply =>
               Left, Right     : Term;
            when Lambda =>
               Name            : String (1 .. Length);
               Expr            : Term;
            when Const_Integer =>
               Integer_Value   : Integer;
            when Const_Float =>
               Float_Value     : Long_Float;
            when Primitive =>
               Primitive_Value : Object;
            when Symbol =>
               Symbol_Name     : String (1 .. Length);
         end case;
      end record;

   function Apply (Left, Right : Term) return Term
   is (new Term_Record'(Apply, 0, Left, Right));

   function Lambda (Name : String; Expr : Term) return Term
   is (new Term_Record'(Lambda, Name'Length, Name, Expr));

   function Combinator (C : Object) return Term
   is (new Term_Record'(Primitive, 0, C));

   function Const (Value : Integer) return Term
   is (new Term_Record'(Const_Integer, 0, Value));

   function Const (Value : Long_Float) return Term
   is (new Term_Record'(Const_Float, 0, Value));

   function Symbol (Name : String) return Term
   is (new Term_Record'(Symbol, Name'Length, Name));

   function Primitive (P : Object) return Term
   is (new Term_Record'(Primitive, 0, P));

   function Is_Application (T : Term) return Boolean
   is (T.Class = Apply);

   function Is_Lambda (T : Term) return Boolean
   is (T.Class = Lambda);

   function Is_Symbol (T : Term) return Boolean
   is (T.Class = Symbol);

   function Is_Atom (T : Term) return Boolean
   is (T.Class in Const_Integer | Const_Float | Primitive);

   function Is_Combinator (T : Term; C : Object) return Boolean
   is (T.Class = Primitive and then T.Primitive_Value = C);

   function Get_Left (T : Term) return Term
   is (T.Left);

   function Get_Right (T : Term) return Term
   is (T.Right);

   function Get_Variable (T : Term) return String
   is (T.Name);

   function Get_Body (T : Term) return Term
   is (T.Expr);

   function Get_Symbol (T : Term) return String
   is (T.Symbol_Name);

   function Get_Atom (T : Term) return Object
   is (case Atom_Class (T.Class) is
          when Const_Integer => To_Object (T.Integer_Value),
          when Const_Float   => To_Object (T.Float_Value),
          when Primitive     => T.Primitive_Value);

end Skit.Terms;
