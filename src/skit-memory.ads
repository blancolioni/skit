private package Skit.Memory is

   --  These accessors are Inline_Always with a documented Pre. With
   --  assertions enabled the inlined body cannot carry the precondition
   --  check, so GNAT warns it is "not enforced"; the Pre is kept for
   --  documentation and static analysis, and the warning is silenced here.
   --  Each body repeats its precondition as a pragma Assert, which an
   --  inlined body does enforce.
   pragma Warnings
     (Off, "aspect ""Pre"" not enforced on inlined subprogram*");

   type Instance (Last : Cell_Address) is limited private;

   --  The heap invariants (ADR 0004). A collector state is Valid when its
   --  two semispaces split the core evenly and the allocation and scan
   --  offsets sit in order inside the active one. A live object is an
   --  application whose cell has been allocated in the active space; a
   --  value may be stored in a cell when it is not an application, or is
   --  live. Heap_Valid says every live cell holds only storable values,
   --  so nothing reachable points into dead space.

   function Valid (This : Instance) return Boolean;

   function Is_Live
     (This : Instance;
      X    : Object)
      return Boolean;

   function Is_Storable
     (This : Instance;
      X    : Object)
      return Boolean
   is (not Is_Application (X) or else Is_Live (This, X));

   function Heap_Valid (This : Instance) return Boolean;
   --  Walks every live cell, so it is checked once per collection, not on
   --  every operation.

   function Left
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always, Pre => Is_Live (This, App);

   function Right
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always, Pre => Is_Live (This, App);

   procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always,
          Pre => Is_Live (This, App) and then Is_Storable (This, To);

   procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always,
          Pre => Is_Live (This, App) and then Is_Storable (This, To);

   function Is_Full
     (This : Instance)
      return Boolean
     with Inline_Always;

   function Live_Cell_Count (This : Instance) return Natural;
   --  Number of cells allocated in the active space; just after a
   --  collection, that is the live set.

   function Append
     (This   : in out Instance;
      Left   : Object;
      Right  : Object)
      return Object
     with Inline_Always,
          Pre  => not Is_Full (This)
                  and then Is_Storable (This, Left)
                  and then Is_Storable (This, Right),
          Post => Live_Cell_Count (This) = Live_Cell_Count (This)'Old + 1
                  and then Is_Live (This, Append'Result)
                  and then Skit.Memory.Left (This, Append'Result) = Left
                  and then Skit.Memory.Right (This, Append'Result) = Right;

   procedure Initialize (This : in out Instance)
     with Pre  => This.Last >= 1 and then This.Last < Cell_Address'Last,
          Post => Valid (This)
                  and then not Is_Full (This)
                  and then Live_Cell_Count (This) = 0;
   --  Last is the index of the final cell, so the core holds Last + 1;
   --  each semispace needs at least one.

   procedure Before_GC (This : in out Instance)
     with Pre  => Valid (This),
          Post => Valid (This) and then Live_Cell_Count (This) = 0;

   procedure After_GC (This : in out Instance)
     with Pre  => Valid (This),
          Post => Valid (This) and then Heap_Valid (This);

   procedure Mark
     (This : in out Instance;
      Root : in out Object)
     with Pre  => Valid (This),
          Post => Valid (This) and then Is_Storable (This, Root);
   --  Between Before_GC and After_GC. A root that is neither live in the
   --  old space nor already copied is a stale pointer, and fails the Post.

   procedure GC (This : in out Instance)
     with Pre  => Valid (This),
          Post => Valid (This);

   procedure Live_Cell
     (This  : Instance;
      Index : Natural;
      Left  : out Object;
      Right : out Object)
     with Pre => Index < Live_Cell_Count (This);
   --  Read the Index'th live cell.  Lets a higher layer walk the live set --
   --  e.g. to discover the foreign objects reachable from it -- without this
   --  access-free core knowing anything about foreign objects or addresses.

private

   type Cell_Type is
      record
         Left, Right : Object;
      end record;

   type Cell_Array is array (Cell_Address range <>) of Cell_Type;

   type Instance (Last : Cell_Address) is limited
      record
         Core              : Cell_Array (0 .. Last);
         Top               : Cell_Address := 0;
         Free              : Cell_Address := 0;
         From_Space        : Cell_Address := 0;
         To_Space          : Cell_Address := 0;
         Space_Size        : Cell_Address := 0;
         Scan              : Cell_Address := 0;
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

   function Valid (This : Instance) return Boolean
   is (This.Space_Size > 0
       and then This.Space_Size = (This.Last + 1) / 2
       and then ((This.To_Space = 0
                  and then This.From_Space = This.Space_Size)
                 or else (This.To_Space = This.Space_Size
                          and then This.From_Space = 0))
       and then This.Top = This.To_Space + This.Space_Size
       and then This.To_Space <= This.Scan
       and then This.Scan <= This.Free
       and then This.Free <= This.Top);

   function Is_Live
     (This : Instance;
      X    : Object)
      return Boolean
   is (Is_Application (X)
       and then Payload (X) >= This.To_Space
       and then Payload (X) < This.Free);

   function Heap_Valid (This : Instance) return Boolean
   is (This.Free = This.To_Space
       or else (for all Address in This.To_Space .. This.Free - 1 =>
                  Is_Storable (This, This.Core (Address).Left)
                  and then Is_Storable (This, This.Core (Address).Right)));
   --  Free = To_Space guards the empty live set: Cell_Address is modular,
   --  so Free - 1 would wrap when both are 0.

   function Live_Cell_Count (This : Instance) return Natural
   is (Natural (This.Free - This.To_Space));

end Skit.Memory;
