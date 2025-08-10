package Skit.Memory is

   type Abstraction is interface;
   type Reference is access all Abstraction'Class;

   function Left
     (This : Abstraction;
      App  : Object)
      return Object
      is abstract;

   function Right
     (This : Abstraction;
      App  : Object)
      return Object
      is abstract;

end Skit.Memory;
