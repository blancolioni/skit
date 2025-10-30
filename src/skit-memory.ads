with Skit.Containers;

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

   procedure Set_Left
     (This : in out Abstraction;
      App  : Object;
      To   : Object)
   is abstract;

   procedure Set_Right
     (This : in out Abstraction;
      App  : Object;
      To   : Object)
   is abstract;

   procedure Add_Container
     (This      : in out Abstraction;
      Container : not null access Skit.Containers.Abstraction'Class)
   is abstract;

   procedure Report (This : Abstraction) is abstract;
   procedure Trace_GC
     (This : in out Abstraction;
      Enabled : Boolean)
   is abstract;

end Skit.Memory;
