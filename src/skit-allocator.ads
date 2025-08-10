with Skit.Memory;

package Skit.Allocator is

   type Abstraction is interface and Skit.Memory.Abstraction;
   type Reference is access all Abstraction'Class;

   function Allocate
     (This   : in out Abstraction;
      Left   : in out Object;
      Right  : in out Object)
      return Object
      is abstract;

end Skit.Allocator;
