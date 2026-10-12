--  The 32-bit tagged object word (ADR 0001, option A): a 30-bit payload
--  and a 2-bit tag. Integers and floats are 30 bits wide.
--
--  This is one of the completions of Skit_Representation; skit.gpr picks
--  one according to the Object_Representation configuration variable.
--  Root Skit derives Object from Word and inherits the operations below;
--  nothing else should depend on this package. Each completion declares
--  the same names, with the same contracts.

package Skit_Representation
  with Pure, SPARK_Mode
is

   Word_Size : constant := 32;

   Tag_Size : constant := 2;

   Tag_Layout : constant := 0;
   --  Identifies this layout in a module image's header: a 2-bit tag
   --  (Integer, Primitive, Application, Float) above a 30-bit payload.

   Payload_Size : constant := 30;

   Max_Integer : constant := 2 ** (Payload_Size - 1) - 1;
   Min_Integer : constant := -2 ** (Payload_Size - 1);

   type Object_Payload is mod 2 ** Payload_Size;
   --  The payload of a primitive or an application.

   type Object_Tag is
     (Integer_Object,
      Primitive_Object,
      Application_Object,
      Float_Object);

   type Word is
      record
         Payload : Object_Payload := 0;
         Tag     : Object_Tag     := Primitive_Object;
      end record
     with Pack, Size => Word_Size;
   --  The default is the primitive with payload 0, which Skit calls Nil.

   function Tag_Of (W : Word) return Object_Tag
   is (W.Tag);

   function Word_Payload (W : Word) return Object_Payload
   is (W.Payload);

   function Make_Word
     (Tag     : Object_Tag;
      Payload : Object_Payload)
      return Word
   is ((Payload => Payload, Tag => Tag))
     with Pre  => Tag in Primitive_Object | Application_Object,
          Post => Tag_Of (Make_Word'Result) = Tag
                  and then Word_Payload (Make_Word'Result) = Payload;

   --  An integer is a two's complement Payload_Size-bit payload. Written
   --  as arithmetic rather than as a conversion to a word and a mask, so
   --  that the round trip can be proved. "mod" with a positive right
   --  operand is never negative.

   function Encode_Integer (X : Integer) return Word
   is ((Payload =>
          Object_Payload (Long_Long_Integer (X) mod 2 ** Payload_Size),
        Tag     => Integer_Object));

   function Decode_Integer (W : Word) return Integer
   is (if W.Payload < 2 ** (Payload_Size - 1)
       then Integer (W.Payload)
       else Integer (W.Payload) - 2 ** Payload_Size)
     with Pre => Tag_Of (W) = Integer_Object;

   --  A float is a Float with its two low mantissa bits dropped.

   function Is_Representable (X : Long_Float) return Boolean
   is (X in Long_Float (Float'First) .. Long_Float (Float'Last));

   function Encode_Float (X : Float) return Word
     with Post => Tag_Of (Encode_Float'Result) = Float_Object;

   function Encode_Long_Float (X : Long_Float) return Word
     with Pre  => Is_Representable (X),
          Post => Tag_Of (Encode_Long_Float'Result) = Float_Object;

   function Decode_Float (W : Word) return Long_Float
     with Pre => Tag_Of (W) = Float_Object;

end Skit_Representation;
