package Skit.Tests is

   type Stack_Operation_Type is private;

   function Push (X : Object) return Stack_Operation_Type;
   function Apply return Stack_Operation_Type;

   function I return Stack_Operation_Type;
   function S return Stack_Operation_Type;
   function K return Stack_Operation_Type;
   function B return Stack_Operation_Type;
   function C return Stack_Operation_Type;
   function Λ return Stack_Operation_Type;

   function Int (X : Integer) return Stack_Operation_Type;
   function Prim (P : Natural) return Stack_Operation_Type;
   function Var (V : Natural) return Stack_Operation_Type;

   type Stack_Operation_Array is
     array (Positive range <>) of Stack_Operation_Type;

   procedure Initialize;

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object);

   procedure Test_Compiler
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object);

   procedure Test
     (Source   : String;
      Expected : Object);

   procedure Test
     (Source   : String;
      Expected : Integer);

   procedure Report;

private

   type Op_Type is (Push, Apply);

   type Stack_Operation_Type is
      record
         Op : Op_Type;
         X  : Object;
      end record;

   function Push (X : Object) return Stack_Operation_Type
   is (Push, X);

   function Apply return Stack_Operation_Type
   is (Apply, Nil);

   function I return Stack_Operation_Type is (Push (Skit.I));
   function S return Stack_Operation_Type is (Push (Skit.S));
   function K return Stack_Operation_Type is (Push (Skit.K));
   function B return Stack_Operation_Type is (Push (Skit.B));
   function C return Stack_Operation_Type is (Push (Skit.C));
   function Λ return Stack_Operation_Type is (Push (Skit.λ));

   function Int (X : Integer) return Stack_Operation_Type
   is (Push (To_Object (X)));

   function Prim (P : Natural) return Stack_Operation_Type
   is (Push,
       (Object_Payload (P) + Primitive_Function_Payload'First,
        Primitive_Object));

   function Var (V : Natural) return Stack_Operation_Type
   is (Push,
       (Object_Payload (V) + Primitive_Variable_Payload'First,
        Primitive_Object));

end Skit.Tests;
