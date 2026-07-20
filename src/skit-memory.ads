private package Skit.Memory
  with SPARK_Mode
is

   type Instance (Last : Cell_Address) is limited private;

   --  Structural invariant of the two-space heap: the semispaces partition
   --  Core, Top/Free/Scan sit inside the current to-space, and Space_Size is
   --  half the arena. Asserted around every collection so a corrupt layout
   --  (Free past Top, a stale semispace base) is caught at the GC boundary
   --  rather than as a downstream wild index. Static_Top is excluded
   --  deliberately: mid-collection it names a from-space address.
   function Valid_Heap (This : Instance) return Boolean
     with Ghost;

   function Left
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always,
          Pre => Is_Application (App) and then App.Payload <= This.Last;

   function Right
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always,
          Pre => Is_Application (App) and then App.Payload <= This.Last;

   procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always,
          Pre => Is_Application (App) and then App.Payload <= This.Last;

   procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always,
          Pre => Is_Application (App) and then App.Payload <= This.Last;

   function Is_Full
     (This : Instance)
      return Boolean
     with Inline_Always;

   procedure Append
     (This    : in out Instance;
      Left    : Object;
      Right   : Object;
      New_App : out Object)
     with Inline_Always, Pre => not Is_Full (This);

   procedure Initialize (This : in out Instance)
     with Pre  => This.Last >= 1,
          Post => not Is_Full (This) and then Valid_Heap (This);

   procedure Before_GC (This : in out Instance)
     with Pre => Valid_Heap (This), Post => Valid_Heap (This);

   procedure After_GC (This : in out Instance)
     with Pre => Valid_Heap (This), Post => Valid_Heap (This);

   procedure Mark
     (This : in out Instance;
      Root : in out Object)
     with Pre => Valid_Heap (This), Post => Valid_Heap (This);

   procedure GC (This : in out Instance)
     with Pre => Valid_Heap (This), Post => Valid_Heap (This);

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

   function Valid_Heap (This : Instance) return Boolean
   is (This.Space_Size = (This.Last + 1) / 2
       and then
         ((This.To_Space = 0 and then This.From_Space = This.Space_Size)
            or else
          (This.To_Space = This.Space_Size and then This.From_Space = 0))
       and then This.Top = This.To_Space + This.Space_Size
       and then This.Free in This.To_Space .. This.Top
       and then This.Scan in This.To_Space .. This.Free);

end Skit.Memory;
