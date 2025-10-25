package Skit.Console is

   type Abstraction is limited interface;

   procedure Put (This : in out Abstraction; S : String) is abstract;
   procedure New_Line (This : in out Abstraction) is abstract;

   procedure Put_Line (This : in out Abstraction'Class; S : String);

end Skit.Console;
