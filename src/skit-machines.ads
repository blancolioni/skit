private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Ordered_Maps;
private with Skit.Memory;

private package Skit.Machines is

   --  Inline_Always subprograms keep a documented Pre that, with assertions
   --  enabled, the inlined body cannot enforce; silence GNAT's "not enforced"
   --  warning while keeping the contract for documentation/static analysis.
   pragma Warnings
     (Off, "aspect ""Pre"" not enforced on inlined subprogram*");

   type Instance (Core_Size : Cell_Address) is tagged limited private;

   procedure Initialize (This : in out Instance'Class);

   function Append
     (This        : in out Instance'Class;
      Left, Right : Object)
      return Object
     with Inline_Always, Pre => Left /= Nil and then Right /= Nil;

   procedure Apply
     (This : in out Instance'Class);

   function Pop
     (This : in out Instance'Class)
      return Object
     with Inline_Always;

   procedure Push
     (This  : in out Instance'Class;
      Value : Object)
     with Inline_Always, Pre => Value /= Nil;

   function Stack_Empty
     (This : Instance'Class)
      return Boolean
     with Inline_Always;

   function Left
     (This : Instance'Class;
      App  : Object)
      return Object;

   function Right
     (This : Instance'Class;
      App  : Object)
      return Object;

   type Lazy_Argument_Array is array (Positive range <>) of Boolean;

   function Primitive
     (This      : in out Instance'Class;
      Primitive : Primitive_Evaluator_Interface'Class)
      return Object;

   procedure Bind
     (This  : in out Instance'Class;
      Name  : Object;
      Value : Object);

   function Lookup
     (This : Instance'Class;
      Name : Object)
      return Object
     with Pre => Is_Symbol (Name);

   procedure Evaluate
     (This      : in out Instance'Class;
      User_Data : access User_Data_Interface'Class);

   function Debug_Image
     (This : Instance'Class;
      X    : Object)
      return String;

private

   package Primitive_Function_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Natural, Primitive_Evaluator_Interface'Class);

   package Environment_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Object_Payload,
        Element_Type => Object,
        "<"          => "<");

   subtype Register is Positive range 1 .. 4;
   --  Shared, temporally, between Eval_Combinator argument slots (R (1 ..
   --  Arg_Count), Arg_Count <= 4) and Advance_Primitive's frame/partial
   --  (R (1), R (2)).  Safe because the two never run concurrently -- see the
   --  Advancing_Primitive assertion in Eval_Combinator.

   type Internal_Register is (Stack, Control, Secondary_Stack);
   type Internal_Register_Array is array (Internal_Register) of Object;

   type Instance (Core_Size : Cell_Address) is tagged limited
      record
         Internal          : Internal_Register_Array := [others => Nil];
         R                 : Object_Array (Register) := [others => Nil];
         Advancing_Primitive : Boolean := False;
         --  True only while Advance_Primitive is on the call stack; asserted
         --  False by Eval_Combinator so the two cannot clobber the shared R.
         Prims             : Primitive_Function_Vectors.Vector;
         Environment       : Environment_Maps.Map;
         Alloc_Count       : Natural := 0;
         Active_Cells      : Natural := 0;
         Max_Active_Cells  : Natural := 0;
         GC_Time           : Duration := 0.0;
         GC_Count          : Natural := 0;
         Eval_Time         : Duration := 0.0;
         Core              : Skit.Memory.Instance (Core_Size);
      end record;

   function Left
     (This : Instance'Class;
      App  : Object)
      return Object
   is (Skit.Memory.Left (This.Core, App));

   function Right
     (This : Instance'Class;
      App  : Object)
      return Object
   is (Skit.Memory.Right (This.Core, App));

end Skit.Machines;
