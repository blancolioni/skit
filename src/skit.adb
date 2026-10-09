with Ada.Unchecked_Conversion;

package body Skit
  with SPARK_Mode
is

   type Transfer_Word is mod 2 ** Integer'Size;

   function To_Float_Word is
     new Ada.Unchecked_Conversion (Float, Transfer_Word);

   --------------
   -- To_Float --
   --------------

   function To_Float (X : Object) return Long_Float
     with SPARK_Mode => Off
   is
      --  Outside SPARK: an arbitrary word need not be a valid Float, so
      --  SPARK rejects Float as the target of an unchecked conversion.
      function To_Float
      is new Ada.Unchecked_Conversion (Transfer_Word, Float);
   begin
      return Long_Float (To_Float (Transfer_Word (X.Payload) * 4));
   end To_Float;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Integer) return Object is
   begin
      --  Two's complement truncated to the payload width, written as
      --  arithmetic rather than as a conversion to a word and a mask, so
      --  that the round trip in the Post can be proved. "mod" with a
      --  positive right operand is never negative.
      return (Object_Payload (Long_Long_Integer (X) mod 2 ** Payload_Size),
              Integer_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Float) return Object is
      W : constant Transfer_Word := To_Float_Word (X);
   begin
      return (Object_Payload (W / 4), Float_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Long_Float) return Object is
   begin
      return To_Object (Float (X));
   end To_Object;

end Skit;
