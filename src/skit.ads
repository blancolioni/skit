package Skit is

   Word_Size    : constant := 32;
   Tag_Size     : constant := 2;
   Payload_Size : constant := Word_Size - Tag_Size;

   Max_Integer : constant := 2 ** (Payload_Size - 1) - 1;
   Min_Integer : constant := -2 ** (Payload_Size - 1);

   type Object is private;
   type Object_Array is array (Positive range <>) of Object;

   function To_Object (X : Integer) return Object;
   function To_Object (X : Float) return Object;
   function To_Object (X : Long_Float) return Object;

   function Is_Integer (X : Object) return Boolean;
   function To_Integer (X : Object) return Integer
     with Pre => Is_Integer (X);

   function Is_Float (X : Object) return Boolean;
   function To_Float (X : Object) return Long_Float
     with Pre => Is_Float (X);

   function Is_Application (X : Object) return Boolean;
   function Is_Symbol (X : Object) return Boolean;
   function Is_Undefined (X : Object) return Boolean;

   type Variable_Index is range 1 .. 999;

   function To_Variable_Object (Index : Variable_Index) return Object;

   type User_Data_Interface is limited interface;

   type Argument_Mode is (Strict, Lazy);
   type Argument_Mode_Array is array (Positive range <>) of Argument_Mode;

   type Primitive_Evaluator_Interface is interface;

   function Argument_Count
     (This : Primitive_Evaluator_Interface)
      return Natural
      is abstract;

   function Argument_Modes
     (This : Primitive_Evaluator_Interface)
      return Argument_Mode_Array
      is abstract
     with Post'Class => Argument_Modes'Result'Length = This.Argument_Count;

   function Evaluate
     (This      : Primitive_Evaluator_Interface;
      User_Data : access User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
      is abstract
     with Pre'Class => Arguments'Length = Argument_Count (This);

private

   type Object_Payload is mod 2 ** Payload_Size;
   subtype Cell_Address is Object_Payload;

   type Object_Tag is
     (Integer_Object,
      Primitive_Object,
      Application_Object,
      Float_Object);

   type Object is
      record
         Payload : Object_Payload := 0;
         Tag     : Object_Tag     := Primitive_Object;
      end record
     with Pack, Size => 32;

   Payload_Nil        : constant Object_Payload := 0;
   Payload_S          : constant Object_Payload := 1;
   Payload_K          : constant Object_Payload := 2;
   Payload_I          : constant Object_Payload := 3;
   Payload_C          : constant Object_Payload := 4;
   Payload_B          : constant Object_Payload := 5;
   Payload_S_Prime    : constant Object_Payload := 6;
   Payload_B_Star     : constant Object_Payload := 7;
   Payload_C_Prime    : constant Object_Payload := 8;
   Payload_Y          : constant Object_Payload := 9;
   Payload_Undefined  : constant Object_Payload := 10;
   Payload_Suspension : constant Object_Payload := 11;

   subtype Combinator_Payload is
     Object_Payload range Payload_S .. Payload_Y;

   Nil        : constant Object := (Payload_Nil, Primitive_Object);
   S          : constant Object := (Payload_S, Primitive_Object);
   K          : constant Object := (Payload_K, Primitive_Object);
   I          : constant Object := (Payload_I, Primitive_Object);
   C          : constant Object := (Payload_C, Primitive_Object);
   B          : constant Object := (Payload_B, Primitive_Object);
   S_Prime    : constant Object := (Payload_S_Prime, Primitive_Object);
   B_Star     : constant Object := (Payload_B_Star, Primitive_Object);
   C_Prime    : constant Object := (Payload_C_Prime, Primitive_Object);
   Y          : constant Object := (Payload_Y, Primitive_Object);
   Undefined  : constant Object := (Payload_Undefined, Primitive_Object);
   Suspension : constant Object := (Payload_Suspension, Primitive_Object);

   subtype Primitive_Function_Payload is
     Object_Payload range 64 .. 4095;

   subtype Primitive_Variable_Payload is
     Object_Payload range 4096 .. 65535;

   function Payload (X : Object) return Object_Payload
   is (X.Payload);

   function Is_Integer (X : Object) return Boolean
   is (X.Tag = Integer_Object);

   function Is_Float (X : Object) return Boolean
   is (X.Tag = Float_Object);

   function To_Variable_Object (Index : Variable_Index) return Object
   is (Object_Payload (Index) + Primitive_Variable_Payload'First,
       Primitive_Object);

   function Is_Primitive (O : Object) return Boolean
   is (O.Tag = Primitive_Object);

   function Is_Application (X : Object) return Boolean
   is (X.Tag = Application_Object);

   function Is_Symbol (X : Object) return Boolean
   is (X.Tag = Primitive_Object
       and then X.Payload in Primitive_Variable_Payload);

   function Is_Undefined (X : Object) return Boolean
   is (X = Undefined);

   function Is_Combinator (X : Object) return Boolean
   is (Is_Primitive (X) and then X.Payload in Combinator_Payload);

   function Is_Primitive_Function (X : Object) return Boolean
   is (Is_Primitive (X) and then X.Payload in Primitive_Function_Payload);

   --  Constructors: the only sanctioned way to build an Object outside the
   --  top-level Skit package, so the tag/payload layout stays private here.

   function Application (Address : Object_Payload) return Object
   is ((Address, Application_Object));

   --  A primitive function is stored as base + its zero-based slot in the
   --  machine's primitive table; the index round-trips through these two.

   function Primitive_Function (Index : Natural) return Object
   is ((Primitive_Function_Payload'First + Object_Payload (Index),
        Primitive_Object));

   function Primitive_Function_Index (X : Object) return Natural
   is (Natural (X.Payload - Primitive_Function_Payload'First));

   --  A symbol is base + its index in the handle's symbol vector.

   function Symbol (Index : Natural) return Object
   is ((Primitive_Variable_Payload'First + Object_Payload (Index),
        Primitive_Object));

   function Symbol_Index (X : Object) return Natural
   is (Natural (X.Payload - Primitive_Variable_Payload'First));

end Skit;
