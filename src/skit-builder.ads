package Skit.Builder is

   type Abstraction is interface;

   function Build_Apply
     (This        : in out Abstraction;
      Left, Right : Object)
      return Object
      is abstract;

end Skit.Builder;
