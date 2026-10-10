private package Skit.Memory
  with SPARK_Mode
is

   --  These accessors are Inline_Always with a documented Pre. With
   --  assertions enabled the inlined body cannot carry the precondition
   --  check, so GNAT warns it is "not enforced"; the Pre is kept for
   --  proof, and the warning is silenced here. Each body repeats the part
   --  of its precondition worth checking at run time as a pragma Assert,
   --  which an inlined body does enforce.
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

   function Cell_Left
     (This : Instance;
      App  : Object)
      return Object
     with Pre => Valid (This) and then Is_Live (This, App);

   function Cell_Right
     (This : Instance;
      App  : Object)
      return Object
     with Pre => Valid (This) and then Is_Live (This, App);
   --  The contents of a live cell, for stating contracts; Left and Right
   --  are what code calls. Not ghost: they are trivial, and ghost entities
   --  below are under a different policy (see Counted).

   function Left
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always,
          Pre  => Valid (This) and then Is_Live (This, App),
          Post => Left'Result = Cell_Left (This, App);

   function Right
     (This : Instance;
      App  : Object)
      return Object
     with Inline_Always,
          Pre  => Valid (This) and then Is_Live (This, App),
          Post => Right'Result = Cell_Right (This, App);

   procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always,
          Pre  => Valid (This)
                  and then Is_Live (This, App)
                  and then Is_Storable (This, To),
          Post => Valid (This);

   procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always,
          Pre  => Valid (This)
                  and then Is_Live (This, App)
                  and then Is_Storable (This, To),
          Post => Valid (This);

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
          Side_Effects,
          Pre  => Valid (This)
                  and then not Is_Full (This)
                  and then Is_Storable (This, Left)
                  and then Is_Storable (This, Right),
          Post => Live_Cell_Count (This) = Live_Cell_Count (This)'Old + 1
                  and then Valid (This)
                  and then Is_Live (This, Append'Result)
                  and then Cell_Left (This, Append'Result) = Left
                  and then Cell_Right (This, Append'Result) = Right;

   procedure Initialize (This : in out Instance)
     with Pre  => This.Last >= 1 and then This.Last < Cell_Address'Last,
          Post => Valid (This)
                  and then not Is_Full (This)
                  and then Live_Cell_Count (This) = 0
                  and then Heap_Valid (This);
   --  Last is the index of the final cell, so the core holds Last + 1;
   --  each semispace needs at least one.

   procedure Live_Cell
     (This  : Instance;
      Index : Natural;
      Left  : out Object;
      Right : out Object)
     with Pre => Valid (This) and then Index < Live_Cell_Count (This);
   --  Read the Index'th live cell.  Lets a higher layer walk the live set --
   --  e.g. to discover the foreign objects reachable from it -- without this
   --  access-free core knowing anything about foreign objects or addresses.

   --  The collection protocol: Before_GC, then Mark on every root, then GC
   --  -- with more Marks and GCs as foreign objects are discovered -- and
   --  finally After_GC.
   --
   --  While a collection is in progress the from-space cells that were
   --  live when it started (the "old heap") are being copied into
   --  to-space. Collecting is the invariant that holds throughout: an
   --  old-heap cell holds old-heap values, except that its Left may have
   --  been overwritten by a forwarding pointer to its live copy; a
   --  to-space cell below Scan holds only live values; and a to-space cell
   --  at or above Scan, copied but not yet scanned, holds old-heap values.
   --  When Scan reaches Free, every live cell holds live values, which is
   --  Heap_Valid -- ADR 0004's property 3.
   --
   --  Collecting walks both spaces, so checking it at run time on every
   --  Mark would make a collection quadratic. These contracts are for
   --  proof only: GNATprove analyses every assertion whatever the
   --  Assertion_Policy, while the compiled code ignores them. The bodies
   --  assert the parts that are cheap: that a root is unmoved on the way
   --  into Mark and storable on the way out, and Heap_Valid at the end of
   --  After_GC.

   function In_Old_Heap
     (This : Instance;
      X    : Object)
      return Boolean;
   --  X is an application into the old heap: the from-space cells that
   --  were live when the current collection began.

   function Is_Unmoved
     (This : Instance;
      X    : Object)
      return Boolean
   is (not Is_Application (X) or else In_Old_Heap (This, X));
   --  X is a value as it stood before the collection: not an application,
   --  or one into the old heap.

   function Collecting (This : Instance) return Boolean;

   function Scan_Complete (This : Instance) return Boolean;
   --  Every copied cell has been scanned.

   --  Counted is what proves that the live set fits in one semispace: the
   --  number of old-heap cells forwarded so far equals the number of
   --  cells copied into to-space. A cell is forwarded at most once, so
   --  while any old-heap cell is unforwarded, fewer than the old heap's
   --  size -- at most Space_Size -- have been copied, and the next copy
   --  fits.
   --
   --  The count is a recursive ghost function, as deep as the old heap is
   --  long: evaluated at run time it would be slow and could overflow the
   --  stack. Ghost code from here on is for proof only.
   pragma Assertion_Policy (Ghost => Ignore);

   function Counted (This : Instance) return Boolean
     with Ghost, Pre => Valid (This);

   pragma Assertion_Policy (Pre => Ignore, Post => Ignore);

   procedure Before_GC (This : in out Instance)
     with Pre  => Heap_Valid (This),
          Post => Collecting (This)
                  and then Counted (This)
                  and then Live_Cell_Count (This) = 0;

   procedure Mark
     (This : in out Instance;
      Root : in out Object)
     with Pre  => Collecting (This)
                  and then Counted (This)
                  and then Is_Unmoved (This, Root),
          Post => Collecting (This)
                  and then Counted (This)
                  and then Is_Storable (This, Root);
   --  A root that is not unmoved -- an application outside the old heap --
   --  is a stale pointer.

   procedure GC (This : in out Instance)
     with Pre  => Collecting (This) and then Counted (This),
          Post => Collecting (This)
                  and then Counted (This)
                  and then Scan_Complete (This);

   procedure After_GC (This : in out Instance)
     with Pre  => Collecting (This) and then Scan_Complete (This),
          Post => Heap_Valid (This);

private

   type Cell_Type is
      record
         Left, Right : Object;
      end record;

   type Cell_Array is array (Cell_Address range <>) of Cell_Type;

   --  Everything the collector counts for Report, and nothing it needs in
   --  order to be correct (ADR 0004 stage 2). The counters are modular, so
   --  none of them can overflow; Static_Top is only ever compared.

   type Statistics is
      record
         Alloc_Count       : Counter := 0;
         Reclaimed         : Counter := 0;
         Copied            : Counter := 0;  --  by the last collection
         Static_Copied     : Counter := 0;
         Transient_Copied  : Counter := 0;
         Static_Top        : Cell_Address := 0;
         --  Free at the end of the last collection: cells below it
         --  survived that collection ("static"), cells above are younger.
         --  Write-barrier instrumentation from ADR 0008: count
         --  Set_Left/Set_Right writes that store a young application
         --  pointer into a static cell -- the old->young references a
         --  generational collector would keep in a remembered set.
         Remembered_Writes : Counter := 0;  --  total over the run
         Epoch_Remembered  : Counter := 0;  --  in the current inter-GC epoch
         Max_Remembered    : Counter := 0;  --  max epoch count seen
      end record;

   type Instance (Last : Cell_Address) is limited
      record
         Core       : Cell_Array (0 .. Last);
         Top        : Cell_Address := 0;
         Free       : Cell_Address := 0;
         From_Space : Cell_Address := 0;
         To_Space   : Cell_Address := 0;
         Space_Size : Cell_Address := 0;
         Scan       : Cell_Address := 0;
         From_Free  : Cell_Address := 0;
         --  The end of the old heap: Free as it was when the current (or
         --  last) collection flipped the spaces.
         Stats      : Statistics;
      end record;

   function Valid (This : Instance) return Boolean
   is (This.Space_Size > 0
       and then This.Space_Size = (This.Last + 1) / 2
       and then ((This.To_Space = 0
                  and then This.From_Space = This.Space_Size)
                 or else (This.To_Space = This.Space_Size
                          and then This.From_Space = 0))
       and then This.Top = This.To_Space + This.Space_Size
       --  Implied by the above, but in arithmetic that does not wrap, which
       --  is what lets a prover see that every index below Top, and every
       --  index in from-space, is inside Core.
       and then Natural (This.Top) <= Natural (This.Last) + 1
       and then Natural (This.Top)
                  = Natural (This.To_Space) + Natural (This.Space_Size)
       and then Natural (This.From_Space) + Natural (This.Space_Size)
                  <= Natural (This.Last) + 1
       and then This.To_Space <= This.Scan
       and then This.Scan <= This.Free
       and then This.Free <= This.Top
       and then This.From_Space <= This.From_Free
       and then Natural (This.From_Free)
                  <= Natural (This.From_Space) + Natural (This.Space_Size));

   function Is_Live
     (This : Instance;
      X    : Object)
      return Boolean
   is (Is_Application (X)
       and then Payload (X) >= This.To_Space
       and then Payload (X) < This.Free);

   --  The quantifiers below range over the whole core with a guard, rather
   --  than over To_Space .. Free - 1: Cell_Address is modular, so Free - 1
   --  would wrap to the top of the address space when Free is 0.

   function Heap_Valid (This : Instance) return Boolean
   is (Valid (This)
       and then
         (for all Address in This.Core'Range =>
            (if Address >= This.To_Space and then Address < This.Free
             then Is_Storable (This, This.Core (Address).Left)
                  and then Is_Storable (This, This.Core (Address).Right))));

   function Cell_Left
     (This : Instance;
      App  : Object)
      return Object
   is (This.Core (Payload (App)).Left);

   function Cell_Right
     (This : Instance;
      App  : Object)
      return Object
   is (This.Core (Payload (App)).Right);

   function Live_Cell_Count (This : Instance) return Natural
   is (Natural (This.Free - This.To_Space));

   function Is_Full
     (This : Instance)
      return Boolean
   is (This.Free = This.Top);

   function In_Old_Heap
     (This : Instance;
      X    : Object)
      return Boolean
   is (Is_Application (X)
       and then Payload (X) >= This.From_Space
       and then Payload (X) < This.From_Free);

   function Collecting (This : Instance) return Boolean
   is (Valid (This)
       and then
         (for all Address in This.Core'Range =>
            (if Address >= This.From_Space and then Address < This.From_Free
             then (Is_Unmoved (This, This.Core (Address).Left)
                   or else Is_Live (This, This.Core (Address).Left))
                  and then Is_Unmoved (This, This.Core (Address).Right)))
       and then
         (for all Address in This.Core'Range =>
            (if Address >= This.To_Space and then Address < This.Scan
             then Is_Storable (This, This.Core (Address).Left)
                  and then Is_Storable (This, This.Core (Address).Right)))
       and then
         (for all Address in This.Core'Range =>
            (if Address >= This.Scan and then Address < This.Free
             then Is_Unmoved (This, This.Core (Address).Left)
                  and then Is_Unmoved (This, This.Core (Address).Right))));

   function Scan_Complete (This : Instance) return Boolean
   is (This.Scan = This.Free);

   function Is_Forwarded_Cell
     (Core     : Cell_Array;
      Address  : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address)
      return Boolean
   is (Is_Application (Core (Address).Left)
       and then Payload (Core (Address).Left) >= To_Space
       and then Payload (Core (Address).Left) < Top)
     with Ghost, Pre => Address in Core'Range;
   --  The cell's Left points into to-space: during a collection, that
   --  makes an old-heap cell forwarded.

   function Count_Forwarded
     (Core     : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address)
      return Natural
   is (if Upto <= From
       then 0
       else Count_Forwarded (Core, From, Upto - 1, To_Space, Top)
            + (if Is_Forwarded_Cell (Core, Upto - 1, To_Space, Top)
               then 1 else 0))
     with Ghost,
          Pre  => From <= Upto
                  and then From >= Core'First
                  and then Natural (Upto) <= Natural (Core'Last) + 1,
          Post => Count_Forwarded'Result <= Natural (Upto - From),
          Subprogram_Variant => (Decreases => Upto);
   --  How many cells in From .. Upto - 1 are forwarded. On arrays, not on
   --  an Instance, so that lemmas can compare the core before and after a
   --  write.

   function Counted (This : Instance) return Boolean
   is (Count_Forwarded
         (This.Core, This.From_Space, This.From_Free,
          This.To_Space, This.Top)
       = Live_Cell_Count (This));

end Skit.Memory;
