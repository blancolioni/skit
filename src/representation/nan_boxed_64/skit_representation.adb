with Ada.Unchecked_Conversion;

package body Skit_Representation
  with SPARK_Mode
is

   --  A float here may be a NaN or an infinity, which validity checking
   --  (-gnatVa) treats as invalid data.
   pragma Validity_Checks (Off);

   Exponent_Mask : constant Word := 16#7FF0_0000_0000_0000#;
   Mantissa_Mask : constant Word := 16#000F_FFFF_FFFF_FFFF#;

   --  Written on the word rather than the float, so that proof can see
   --  that a word which is not a NaN lies outside the boxes.

   function Is_NaN (W : Word) return Boolean
   is ((W and Exponent_Mask) = Exponent_Mask
       and then (W and Mantissa_Mask) /= 0);

   function To_Word is
     new Ada.Unchecked_Conversion (Long_Float, Word);

   ------------------
   -- Decode_Float --
   ------------------

   function Decode_Float (W : Word) return Long_Float
     with SPARK_Mode => Off
   is
      --  Outside SPARK: an arbitrary word need not be a valid Long_Float,
      --  so SPARK rejects Long_Float as the target of an unchecked
      --  conversion.
      function To_Long_Float
      is new Ada.Unchecked_Conversion (Word, Long_Float);
   begin
      return To_Long_Float (W);
   end Decode_Float;

   ------------------
   -- Encode_Float --
   ------------------

   function Encode_Float (X : Float) return Word is
   begin
      return Encode_Long_Float (Long_Float (X));
   end Encode_Float;

   -----------------------
   -- Encode_Long_Float --
   -----------------------

   function Encode_Long_Float (X : Long_Float) return Word is
      W : constant Word := To_Word (X);
   begin
      if Is_NaN (W) then
         return Canonical_NaN;
      else
         return W;
      end if;
   end Encode_Long_Float;

   ----------------------
   -- Is_Representable --
   ----------------------

   function Is_Representable (X : Long_Float) return Boolean is
      pragma Unreferenced (X);
   begin
      return True;
   end Is_Representable;

end Skit_Representation;
