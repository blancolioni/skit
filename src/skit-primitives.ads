package Skit.Primitives is

   type Abstraction is interface;

   function Argument_Count
     (This : Abstraction)
      return Natural
      is abstract;

   function Is_Lazy
     (This : Abstraction;
      Arg_Index : Positive)
      return Boolean
      is abstract
     with Pre'Class => Arg_Index <= This.Argument_Count;

   function Evaluate
     (This      : Abstraction;
      Arguments : Object_Array)
      return Object
      is abstract;

end Skit.Primitives;
