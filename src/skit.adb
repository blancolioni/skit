with Ada.Unchecked_Conversion;

package body Skit is

   --  NaN-boxing reinterprets bit patterns as Long_Float, including genuine
   --  NaNs. Under -gnatVa a NaN reads as "invalid data" and raises, so the
   --  validity check is meaningless for the reinterpret paths here (ADR 0001
   --  flags exactly this). Suppress it within this body only.
   pragma Suppress (Validity_Check);

   --  Object and Long_Float are both 64-bit; NaN-boxing reinterprets the bits.
   function To_Bits is
     new Ada.Unchecked_Conversion (Long_Float, Object);

   function To_Double is
     new Ada.Unchecked_Conversion (Object, Long_Float);

   function To_Bits is
     new Ada.Unchecked_Conversion (Long_Long_Integer, Object);

   function To_Signed is
     new Ada.Unchecked_Conversion (Object, Long_Long_Integer);

   Sign_Extend : constant Object := 16#FFFF_0000_0000_0000#;
   Sign_Bit    : constant Object := 2 ** (Integer_Bits - 1);

   --  A positive quiet NaN: the value To_Float hands back for the canonical
   --  Float box, and never itself a box (sign bit clear).
   NaN_Value : constant Long_Float := To_Double (16#7FF8_0000_0000_0000#);

   --------------
   -- To_Float --
   --------------

   function To_Float (X : Object) return Long_Float is
   begin
      if Is_Boxed (X) then
         return NaN_Value;   --  the canonical Float_NaN box
      else
         return To_Double (X);
      end if;
   end To_Float;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : Object) return Integer is
      Bits : Object := X and Payload_Mask;
   begin
      if (Bits and Sign_Bit) /= 0 then
         Bits := Bits or Sign_Extend;
      end if;
      return Integer (To_Signed (Bits));
   end To_Integer;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Integer) return Object is
      Bits : constant Object := To_Bits (Long_Long_Integer (X));
   begin
      return Make_Integer (Object_Payload (Bits and Payload_Mask));
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Float) return Object is
   begin
      return To_Object (Long_Float (X));
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Long_Float) return Object is
      Bits : constant Object := To_Bits (X);
   begin
      --  A double that lands in the box pattern is a negative quiet NaN;
      --  fold it to the single canonical Float box so it cannot alias a
      --  tagged value.
      if (Bits and Box_Mask) = Box_Sig then
         return Float_NaN;
      else
         return Bits;
      end if;
   end To_Object;

end Skit;
