package Skit is

   --  NaN-boxing prototype (ADR 0001, option C). Object is a 64-bit word:
   --  a genuine IEEE-754 double is stored raw; every non-float value lives in
   --  the reserved negative-quiet-NaN pattern space. See the private part for
   --  the encoding and Skit body for the conversions.

   Word_Size : constant := 64;

   --  The box payload is 48 bits, but integer values round-trip through Ada's
   --  32-bit Standard.Integer, so the language integer range stays 32-bit.
   Integer_Bits : constant := 48;

   Max_Integer : constant := 2 ** 31 - 1;
   Min_Integer : constant := -2 ** 31;

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

   --  ------------------------------------------------------------------
   --  NaN-box encoding
   --
   --  A 64-bit IEEE-754 double is a "box" (a tagged non-float value) iff
   --  sign = 1, exponent = all-ones and the quiet bit (mantissa bit 51) = 1
   --  -- i.e. a negative quiet NaN, top 13 bits = 0x1FFF. That leaves 51
   --  free low bits: 2 for a tag (bits 49..48) and 48 for a payload.
   --
   --    tag 0 = Integer   tag 1 = Primitive
   --    tag 2 = Application   tag 3 = Float (the single reserved NaN)
   --
   --  Any word NOT matching the box pattern is a genuine double (finite,
   --  +/-inf, +NaN, -inf, -sNaN all keep quiet bit 0 or sign 0). A double
   --  that IS a negative quiet NaN (e.g. the x86 0.0/0.0 result) would alias
   --  the box space, so To_Object canonicalises every float NaN to the tag-3
   --  box; To_Float turns that box back into a real NaN.
   --  ------------------------------------------------------------------

   type Object is mod 2 ** 64
     with Default_Value => 16#FFF9_0000_0000_0000#;  --  = Nil (Primitive 0)

   type Object_Payload is mod 2 ** Integer_Bits;
   subtype Cell_Address is Object_Payload;

   Box_Sig      : constant Object := 16#FFF8_0000_0000_0000#;
   Box_Mask     : constant Object := 16#FFF8_0000_0000_0000#;
   Payload_Mask : constant Object := 16#0000_FFFF_FFFF_FFFF#;
   Two_48       : constant Object := 2 ** 48;

   type Object_Tag is
     (Integer_Object,
      Primitive_Object,
      Application_Object,
      Float_Object);

   function Is_Boxed (O : Object) return Boolean
   is ((O and Box_Mask) = Box_Sig);

   function Tag (O : Object) return Object_Tag
   is (if not Is_Boxed (O) then Float_Object
       elsif (O / Two_48) mod 4 = 0 then Integer_Object
       elsif (O / Two_48) mod 4 = 1 then Primitive_Object
       elsif (O / Two_48) mod 4 = 2 then Application_Object
       else Float_Object);

   function Payload (O : Object) return Object_Payload
   is (Object_Payload (O and Payload_Mask));

   function Address (O : Object) return Cell_Address
   is (Cell_Address (Payload (O)));

   function Box (Code : Object; Value : Object_Payload) return Object
   is (Box_Sig or (Code * Two_48) or (Object (Value) and Payload_Mask));

   function Make_Integer (Value : Object_Payload) return Object
   is (Box (0, Value));

   function Make_Primitive (Value : Object_Payload) return Object
   is (Box (1, Value));

   function Make_Application (Addr : Cell_Address) return Object
   is (Box (2, Object_Payload (Addr)));

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

   Nil        : constant Object := Make_Primitive (Payload_Nil);
   S          : constant Object := Make_Primitive (Payload_S);
   K          : constant Object := Make_Primitive (Payload_K);
   I          : constant Object := Make_Primitive (Payload_I);
   C          : constant Object := Make_Primitive (Payload_C);
   B          : constant Object := Make_Primitive (Payload_B);
   S_Prime    : constant Object := Make_Primitive (Payload_S_Prime);
   B_Star     : constant Object := Make_Primitive (Payload_B_Star);
   C_Prime    : constant Object := Make_Primitive (Payload_C_Prime);
   Y          : constant Object := Make_Primitive (Payload_Y);
   Undefined  : constant Object := Make_Primitive (Payload_Undefined);
   Suspension : constant Object := Make_Primitive (Payload_Suspension);

   --  Canonical box for a genuine float NaN (tag 3).
   Float_NaN  : constant Object := Box (3, 0);

   subtype Primitive_Function_Payload is
     Object_Payload range 64 .. 4095;

   subtype Primitive_Variable_Payload is
     Object_Payload range 4096 .. 65535;

   function Payload (X : Object) return Object_Payload
   is (X.Payload);

   function Is_Integer (X : Object) return Boolean
   is (Tag (X) = Integer_Object);

   function Is_Float (X : Object) return Boolean
   is (Tag (X) = Float_Object);

   function To_Variable_Object (Index : Variable_Index) return Object
   is (Make_Primitive
         (Object_Payload (Index) + Primitive_Variable_Payload'First));

   function Is_Primitive (O : Object) return Boolean
   is (Tag (O) = Primitive_Object);

   --  top 16: sig+quiet+tag
   App_Mask : constant Object := 16#FFFF_0000_0000_0000#;

   --  box sig | tag 2
   App_Sig  : constant Object := 16#FFFA_0000_0000_0000#;

   function Is_Application (X : Object) return Boolean
   is ((X and App_Mask) = App_Sig);

   function Is_Symbol (X : Object) return Boolean
   is (Is_Primitive (X)
       and then Payload (X) in Primitive_Variable_Payload);

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
