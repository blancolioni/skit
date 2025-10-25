package Skit.Stacks is

   type Abstraction is interface;

   procedure Push
     (This   : in out Abstraction;
      Values : in out Object_Array)
   is abstract;

   function Pop
     (This   : in out Abstraction;
      Values : out Object_Array)
      return Boolean
      is abstract;

   function Top
     (This : Abstraction)
      return Object
      is abstract;

   procedure Apply
     (This : in out Abstraction)
   is abstract;

   procedure Push
     (This  : in out Abstraction'Class;
      Value : Object);

   procedure Pop
     (This  : in out Abstraction'Class;
      Value : out Object);

   function Pop
     (This : in out Abstraction'Class)
      return Object;

   procedure Drop
     (This : in out Abstraction'Class);

end Skit.Stacks;
