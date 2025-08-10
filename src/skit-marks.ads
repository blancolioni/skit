package Skit.Marks is

   type Abstraction is interface;

   procedure Mark
     (This : in out Abstraction;
      Set  : not null access procedure (Marked_Object : in out Object))
   is abstract;

end Skit.Marks;
