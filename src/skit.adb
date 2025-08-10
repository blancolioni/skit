with Ada.Unchecked_Conversion;

package body Skit is

   type Transfer_Word is mod 2 ** Integer'Size;

   function To_Integer_Word is
     new Ada.Unchecked_Conversion (Integer, Transfer_Word);

   function To_Float_Word is
     new Ada.Unchecked_Conversion (Float, Transfer_Word);

   function To_Integer
   is new Ada.Unchecked_Conversion (Transfer_Word, Integer);

   function To_Float
   is new Ada.Unchecked_Conversion (Transfer_Word, Float);

   --------------
   -- To_Float --
   --------------

   function To_Float (X : Object) return Float is
   begin
      return To_Float (Transfer_Word (X.Payload) * 4);
   end To_Float;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : Object) return Integer is
   begin
      return To_Integer (Transfer_Word (X.Payload));
   end To_Integer;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Integer) return Object is
      W : constant Transfer_Word := To_Integer_Word (X);
   begin
      return (Object_Payload (W), Integer_Object);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (X : Float) return Object is
      W : constant Transfer_Word := To_Float_Word (X);
   begin
      return (Object_Payload (W / 4), Float_Object);
   end To_Object;

end Skit;
