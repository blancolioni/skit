package body Skit.Console is

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : in out Abstraction'Class; S : String) is
   begin
      This.Put (S);
      This.New_Line;
   end Put_Line;

end Skit.Console;
