package Skit is

   Word_Size    : constant := 32;
   Tag_Size     : constant := 2;
   Payload_Size : constant := Word_Size - Tag_Size;

   Max_Integer : constant := 2 ** (Payload_Size - 1) - 1;
   Min_Integer : constant := -2 ** (Payload_Size - 1);

   type Object is private;
   type Object_Array is array (Positive range <>) of Object;

   Nil       : constant Object;
   S         : constant Object;
   K         : constant Object;
   I         : constant Object;
   C         : constant Object;
   B         : constant Object;
   S_Prime   : constant Object;
   B_Star    : constant Object;
   C_Prime   : constant Object;
   Lambda    : constant Object;
   Undefined : constant Object;

   function To_Object (X : Integer) return Object;
   function To_Object (X : Float) return Object;

   type Variable_Index is range 1 .. 999;

   function To_Variable_Object (Index : Variable_Index) return Object;

private

   type Object_Payload is mod 2 ** Payload_Size;

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

   Payload_Nil       : constant Object_Payload := 0;
   Payload_S         : constant Object_Payload := 1;
   Payload_K         : constant Object_Payload := 2;
   Payload_I         : constant Object_Payload := 3;
   Payload_C         : constant Object_Payload := 4;
   Payload_B         : constant Object_Payload := 5;
   Payload_S_Prime   : constant Object_Payload := 6;
   Payload_B_Star    : constant Object_Payload := 7;
   Payload_C_Prime   : constant Object_Payload := 8;
   Payload_Lambda    : constant Object_Payload := 9;
   Payload_Undefined : constant Object_Payload := 10;

   Nil       : constant Object := (Payload_Nil, Primitive_Object);
   S         : constant Object := (Payload_S, Primitive_Object);
   K         : constant Object := (Payload_K, Primitive_Object);
   I         : constant Object := (Payload_I, Primitive_Object);
   C         : constant Object := (Payload_C, Primitive_Object);
   B         : constant Object := (Payload_B, Primitive_Object);
   S_Prime   : constant Object := (Payload_S_Prime, Primitive_Object);
   B_Star    : constant Object := (Payload_B_Star, Primitive_Object);
   C_Prime   : constant Object := (Payload_C_Prime, Primitive_Object);
   Lambda    : constant Object := (Payload_Lambda, Primitive_Object);
   Undefined : constant Object := (Payload_Undefined, Primitive_Object);

   subtype Primitive_Function_Payload is
     Object_Payload range 64 .. 255;

   subtype Primitive_Variable_Payload is
     Object_Payload range 4096 .. 65535;

   function To_Integer (X : Object) return Integer;
   function To_Float (X : Object) return Float;

   function To_Variable_Object (Index : Variable_Index) return Object
   is (Object_Payload (Index) + Primitive_Variable_Payload'First,
       Primitive_Object);

   function Is_Primitive (O : Object) return Boolean
   is (O.Tag = Primitive_Object);

end Skit;
