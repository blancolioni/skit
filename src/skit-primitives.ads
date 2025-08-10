package Skit.Primitives is

   type Abstraction is interface;

   function Argument_Count
     (This : Abstraction)
      return Natural
      is abstract;

   function Evaluate
     (This      : Abstraction;
      Arguments : Object_Array)
      return Object
      is abstract;

end Skit.Primitives;
