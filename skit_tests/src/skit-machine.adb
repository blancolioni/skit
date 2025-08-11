with Skit.Debug;

package body Skit.Machine is

   --------------
   -- Show_Top --
   --------------

   function Show_Top
     (This : Abstraction'Class)
      return String
   is
   begin
      return Skit.Debug.Image (This.Top, This'Access);
   end Show_Top;

end Skit.Machine;
