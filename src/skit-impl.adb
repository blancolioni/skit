with Skit.Impl.Machines;

package body Skit.Impl is

   -------------
   -- Machine --
   -------------

   function Machine
     (Size      : Natural)
      return Skit.Machine.Reference
   is
   begin
      return Skit.Impl.Machines.Create (Size);
   end Machine;

end Skit.Impl;
