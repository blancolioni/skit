with Skit.Stacks;

package Skit.Primitives is

   type Abstraction is interface;

   function Name
     (This : Abstraction)
      return String
      is abstract;

   function Argument_Count
     (This : Abstraction)
      return Natural
      is abstract;

   procedure Evaluate
     (This    : Abstraction;
      Stack   : in out Skit.Stacks.Abstraction'Class)
   is abstract;

end Skit.Primitives;
