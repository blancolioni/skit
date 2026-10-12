with Ada.Unchecked_Conversion;

package body Skit_Representation
  with SPARK_Mode
is

   type Transfer_Word is mod 2 ** Float'Size;

   function To_Float_Word is
     new Ada.Unchecked_Conversion (Float, Transfer_Word);

   ------------------
   -- Decode_Float --
   ------------------

   function Decode_Float (W : Word) return Long_Float
     with SPARK_Mode => Off
   is
      --  Outside SPARK: an arbitrary word need not be a valid Float, so
      --  SPARK rejects Float as the target of an unchecked conversion.
      function To_Float
      is new Ada.Unchecked_Conversion (Transfer_Word, Float);
   begin
      return Long_Float (To_Float (Transfer_Word (W.Payload) * 4));
   end Decode_Float;

   ------------------
   -- Encode_Float --
   ------------------

   function Encode_Float (X : Float) return Word is
      W : constant Transfer_Word := To_Float_Word (X);
   begin
      return (Payload => Object_Payload (W / 4), Tag => Float_Object);
   end Encode_Float;

   -----------------------
   -- Encode_Long_Float --
   -----------------------

   function Encode_Long_Float (X : Long_Float) return Word is
   begin
      return Encode_Float (Float (X));
   end Encode_Long_Float;

end Skit_Representation;
