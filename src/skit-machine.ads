with Skit.Evaluator;
with Skit.Memory;
with Skit.Primitives;
with Skit.Stacks;

package Skit.Machine is

   type Abstraction is interface
     and Skit.Evaluator.Abstraction
     and Skit.Memory.Abstraction
     and Skit.Stacks.Abstraction;
   type Reference is access all Abstraction'Class;

   function Bind
     (This      : in out Abstraction;
      Primitive : Skit.Primitives.Abstraction'Class)
     return Object
   is abstract;

   type Temporary is range 0 .. 15;

   procedure Set
     (This  : in out Abstraction;
      T     : Temporary;
      Value : Object)
   is abstract;

   function Get
     (This  : Abstraction;
      T     : Temporary)
      return Object
   is abstract;

   --  function Show_Top
   --    (This : Abstraction'Class)
   --     return String;
   --
end Skit.Machine;
