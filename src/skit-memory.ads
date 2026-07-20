private package Skit.Memory is

   --  These accessors are Inline_Always with a documented Pre. With
   --  assertions enabled the inlined body cannot carry the precondition
   --  check, so GNAT warns it is "not enforced"; the Pre is kept for
   --  documentation and static analysis, and the warning is silenced here.
   pragma Warnings
     (Off, "aspect ""Pre"" not enforced on inlined subprogram*");

   type Instance (Last : Cell_Address) is limited private;

   function Left
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always, Pre => Is_Application (App);

   function Right
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always, Pre => Is_Application (App);

   procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always, Pre => Is_Application (App);

   procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always, Pre => Is_Application (App);

   function Is_Full
     (This : Instance)
      return Boolean
     with Inline_Always;

   function Append
     (This   : in out Instance;
      Left   : Object;
      Right  : Object)
      return Object
     with Inline_Always, Pre => not Is_Full (This);

   procedure Initialize (This : in out Instance)
     with Post => not Is_Full (This);

   procedure Before_GC (This : in out Instance);
   procedure After_GC (This : in out Instance);

   procedure Mark
     (This : in out Instance;
      Root : in out Object);

   procedure GC (This : in out Instance);

private

   type Cell_Type is
      record
         Left, Right : Object;
      end record;

   type Cell_Array is array (Cell_Address range <>) of Cell_Type;

   type Instance (Last : Cell_Address) is limited
      record
         Core              : Cell_Array (0 .. Last);
         Top               : Cell_Address;
         Free              : Cell_Address;
         From_Space        : Cell_Address;
         To_Space          : Cell_Address;
         Space_Size        : Cell_Address;
         Scan              : Cell_Address;
         Copied            : Natural := 0;
         Static_Copied     : Natural := 0;
         Transient_Copied  : Natural := 0;
         Alloc_Count       : Natural := 0;
         Reclaimed         : Natural := 0;
         Static_Top        : Cell_Address := 0;
         --  Write-barrier instrumentation: count Set_Left/Set_Right writes
         --  that store a young (this-epoch) application pointer into a static
         --  (survived-last-GC) cell -- i.e. the old->young references a
         --  generational nursery collector would keep in a remembered set.
         Remembered_Writes : Natural := 0;  --  total over the run
         Epoch_Remembered  : Natural := 0;  --  in the current inter-GC epoch
         Max_Remembered    : Natural := 0;  --  max epoch count seen
      end record;

end Skit.Memory;
