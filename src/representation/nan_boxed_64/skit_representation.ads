--  The NaN-boxed 64-bit object word (ADR 0001, option C). A float is an
--  IEEE double, stored as its own bits. Everything else is boxed in the
--  NaN space: the top 16 bits are a box tag, which no float uses, and the
--  low bits hold the value.
--
--     16#FFF9# & 16 zero bits & 32-bit integer   integer
--     16#FFFA# & 18 zero bits & 30-bit payload   primitive
--     16#FFFB# & 18 zero bits & 30-bit payload   application
--     anything else                             float
--
--  Every NaN is stored as Canonical_NaN, so no float lands in a box.
--  Boxed words must only be moved, never used in float arithmetic: the
--  FPU may quiet or replace a NaN, and with it the boxed value.
--
--  This is one of the completions of Skit_Representation; skit.gpr picks
--  one according to the Object_Representation configuration variable.
--  Root Skit derives Object from Word and inherits the operations below;
--  nothing else should depend on this package. Each completion declares
--  the same names, with the same contracts.

package Skit_Representation
  with Pure, SPARK_Mode
is

   Word_Size : constant := 64;

   Tag_Size : constant := 16;

   Tag_Layout : constant := 1;
   --  Identifies this layout in a module image's header: NaN-boxed, as
   --  above.

   Payload_Size : constant := 30;

   Max_Integer : constant := 2 ** 31 - 1;
   Min_Integer : constant := -2 ** 31;

   type Object_Payload is mod 2 ** Payload_Size;
   --  The payload of a primitive or an application. Kept as narrow as the
   --  32-bit word's, so that cell addresses fit in Natural (ADR 0001,
   --  "Interaction with the SPARK proof").

   type Object_Tag is
     (Integer_Object,
      Primitive_Object,
      Application_Object,
      Float_Object);

   Box_Unit        : constant := 2 ** 48;
   Integer_Box     : constant := 16#FFF9#;
   Primitive_Box   : constant := 16#FFFA#;
   Application_Box : constant := 16#FFFB#;

   Canonical_NaN : constant := 16#7FF8_0000_0000_0000#;
   --  The quiet NaN with no payload bits and the sign bit clear. x86
   --  produces the negative one for 0.0 / 0.0, which is just as far from
   --  the boxes, but every NaN is stored as this one.

   type Word is mod 2 ** Word_Size
     with Default_Value => Primitive_Box * Box_Unit;
   --  The default is the primitive with payload 0, which Skit calls Nil.

   function Tag_Of (W : Word) return Object_Tag
   is (case W / Box_Unit is
          when Integer_Box     => Integer_Object,
          when Primitive_Box   => Primitive_Object,
          when Application_Box => Application_Object,
          when others          => Float_Object);

   function Word_Payload (W : Word) return Object_Payload
   is (Object_Payload (W mod 2 ** Payload_Size));

   function Make_Word
     (Tag     : Object_Tag;
      Payload : Object_Payload)
      return Word
   is ((if Tag = Primitive_Object then Primitive_Box else Application_Box)
       * Box_Unit + Word (Payload))
     with Pre  => Tag in Primitive_Object | Application_Object,
          Post => Tag_Of (Make_Word'Result) = Tag
                  and then Word_Payload (Make_Word'Result) = Payload;

   --  An integer is the low 32 bits, two's complement, so every Integer
   --  round-trips.

   function Encode_Integer (X : Integer) return Word
   is (Integer_Box * Box_Unit + Word (Long_Long_Integer (X) mod 2 ** 32));

   function Decode_Integer (W : Word) return Integer
   is (if W mod 2 ** 32 < 2 ** 31
       then Integer (W mod 2 ** 32)
       else Integer (Long_Long_Integer (W mod 2 ** 32) - 2 ** 32))
     with Pre => Tag_Of (W) = Integer_Object;

   --  A float is a Long_Float, stored as its own bits.

   function Is_Representable (X : Long_Float) return Boolean
     with Post => Is_Representable'Result;

   function Encode_Float (X : Float) return Word
     with Post => Tag_Of (Encode_Float'Result) = Float_Object;

   function Encode_Long_Float (X : Long_Float) return Word
     with Pre  => Is_Representable (X),
          Post => Tag_Of (Encode_Long_Float'Result) = Float_Object;

   function Decode_Float (W : Word) return Long_Float
     with Pre => Tag_Of (W) = Float_Object;

end Skit_Representation;
