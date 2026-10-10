package body Skit.Memory
  with SPARK_Mode
is

   --  Compile-time gate for poisoning the dead from-space after each
   --  collection. When False (the default), the constant folds and the
   --  guarded block is dead-code-eliminated. The heap-integrity check that
   --  used to sit behind this flag is now the Heap_Valid contract on
   --  After_GC.
   Poison_Dead_Space : constant Boolean := False;

   --  Poison value written over every dead from-space cell after a collection.
   --  An application pointer to the top of the address space: it lands outside
   --  any live semispace, so a stale read of a reclaimed cell either trips the
   --  heap check or faults on the out-of-range index instead of silently
   --  returning plausible-looking garbage.
   Invalid : constant Object := Application (Cell_Address'Last);

   procedure Poison_From_Space (This : in out Instance)
     with Pre => Valid (This);

   function In_From_Space
     (This   : Instance;
      Item   : Object)
      return Boolean
   is (Is_Application (Item)
         and then Payload (Item) in
           This.From_Space .. This.From_Space + This.Space_Size - 1);

   function In_To_Space
     (This   : Instance;
      Item   : Object)
      return Boolean
   is (Is_Application (Item)
       and then Payload (Item) in
         This.To_Space .. This.To_Space + This.Space_Size - 1);

   function Same_Layout
     (This       : Instance;
      Top        : Cell_Address;
      To_Space   : Cell_Address;
      From_Space : Cell_Address)
      return Boolean
   is (This.Top = Top
       and then This.To_Space = To_Space
       and then This.From_Space = From_Space);
   --  The semispaces have not moved. Stating it lets a prover rebuild
   --  Valid by equalities after Copy and Move, instead of from scratch.
   --  Not ghost: the spec's Ghost => Ignore policy carries over into this
   --  body, and this is too cheap to be worth keeping out of the build.

   --  Lemmas about Count_Forwarded, for the proof that the live set fits
   --  in one semispace (see Counted in the spec). Each is proved by
   --  induction on Upto, and each walks the whole old heap, so they are
   --  ghost and ignored at run time.
   pragma Assertion_Policy (Ghost => Ignore);

   function Counting_Range
     (Core : Cell_Array;
      From : Cell_Address;
      Upto : Cell_Address)
      return Boolean
   is (From <= Upto
       and then From >= Core'First
       and then Natural (Upto) <= Natural (Core'Last) + 1)
     with Ghost;
   --  The precondition of Count_Forwarded.

   procedure Lemma_None_Forwarded
     (Core     : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address)
     with Ghost,
          Pre  => Counting_Range (Core, From, Upto)
                  and then (for all A in Core'Range =>
                              (if A >= From and then A < Upto
                               then not Is_Forwarded_Cell
                                          (Core, A, To_Space, Top))),
          Post => Count_Forwarded (Core, From, Upto, To_Space, Top) = 0,
          Subprogram_Variant => (Decreases => Upto);
   --  Nothing forwarded counts as nothing.

   procedure Lemma_Same_Lefts
     (Before   : Cell_Array;
      After    : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address)
     with Ghost,
          Pre  => Before'First = After'First
                  and then Before'Last = After'Last
                  and then Counting_Range (Before, From, Upto)
                  and then (for all A in Before'Range =>
                              (if A >= From and then A < Upto
                               then Before (A).Left = After (A).Left)),
          Post => Count_Forwarded (After, From, Upto, To_Space, Top)
                    = Count_Forwarded (Before, From, Upto, To_Space, Top),
          Subprogram_Variant => (Decreases => Upto);
   --  Writing cells outside the range leaves the count alone.

   procedure Lemma_Forward_One
     (Before   : Cell_Array;
      After    : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address;
      Address  : Cell_Address)
     with Ghost,
          Pre  => Before'First = After'First
                  and then Before'Last = After'Last
                  and then Counting_Range (Before, From, Upto)
                  and then Address >= From
                  and then Address < Upto
                  and then not Is_Forwarded_Cell
                                 (Before, Address, To_Space, Top)
                  and then Is_Forwarded_Cell (After, Address, To_Space, Top)
                  and then (for all A in Before'Range =>
                              (if A >= From and then A < Upto
                                 and then A /= Address
                               then Before (A).Left = After (A).Left)),
          Post => Count_Forwarded (After, From, Upto, To_Space, Top)
                    = Count_Forwarded (Before, From, Upto, To_Space, Top) + 1,
          Subprogram_Variant => (Decreases => Upto);
   --  Forwarding one more cell adds one.

   procedure Lemma_Unforwarded_Bound
     (Core     : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address;
      Address  : Cell_Address)
     with Ghost,
          Pre  => Counting_Range (Core, From, Upto)
                  and then Address >= From
                  and then Address < Upto
                  and then not Is_Forwarded_Cell
                                 (Core, Address, To_Space, Top),
          Post => Count_Forwarded (Core, From, Upto, To_Space, Top)
                    < Natural (Upto - From),
          Subprogram_Variant => (Decreases => Upto);
   --  While one cell is unforwarded, not all of them are.

   procedure Lemma_Room (This : Instance)
     with Ghost,
          Pre  => Valid (This)
                  and then Live_Cell_Count (This)
                             < Natural (This.From_Free - This.From_Space),
          Post => This.Free < This.Top;
   --  Fewer cells copied than the old heap holds leaves room for another:
   --  the old heap is no bigger than a semispace.

   --  Copy and Move maintain Collecting, which walks both spaces: checked
   --  on every call it would make a collection quadratic. Flip's Post
   --  takes a copy of the whole core with 'Old. As for the collection
   --  protocol in the spec, these contracts are for proof only.
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore);

   procedure Flip (This : in out Instance)
     with Pre  => Valid (This),
          Post => Valid (This)
                  and then This.To_Space = This.From_Space'Old
                  and then This.From_Space = This.To_Space'Old
                  and then This.From_Free = This.Free'Old
                  and then This.Free = This.To_Space
                  and then This.Scan = This.To_Space
                  and then This.Core = This.Core'Old;

   procedure Copy
     (This        : in out Instance;
      Address     : Cell_Address;
      New_Address : out Cell_Address)
     with Pre  => Collecting (This)
                  and then This.Free < This.Top
                  and then In_Old_Heap (This, Application (Address))
                  and then Is_Unmoved (This, This.Core (Address).Left),
          Post => Same_Layout (This, This.Top'Old, This.To_Space'Old,
                               This.From_Space'Old)
                  and then Collecting (This)
                  and then New_Address = This.Free'Old
                  and then This.Free = This.Free'Old + 1
                  and then This.Scan = This.Scan'Old
                  and then This.From_Free = This.From_Free'Old
                  and then This.Core
                             = (This.Core'Old with delta
                                  New_Address => This.Core'Old (Address))
                  and then This.Core (Address) = This.Core'Old (Address);
   --  Copy an old-heap cell that has not been copied yet to the end of
   --  to-space. The live set has to fit in one semispace; see Move.

   procedure Forward_Copy
     (This        : in out Instance;
      Address     : Cell_Address;
      New_Address : out Cell_Address)
     with Pre  => Collecting (This)
                  and then Counted (This)
                  and then This.Free < This.Top
                  and then In_Old_Heap (This, Application (Address))
                  and then not In_To_Space (This, This.Core (Address).Left),
          Post => Same_Layout (This, This.Top'Old, This.To_Space'Old,
                               This.From_Space'Old)
                  and then Collecting (This)
                  and then Counted (This)
                  and then New_Address = This.Free'Old
                  and then This.Free = This.Free'Old + 1
                  and then This.Scan = This.Scan'Old
                  and then This.From_Free = This.From_Free'Old
                  and then This.Core (Address).Left
                             = Application (New_Address);
   --  Copy an unforwarded old-heap cell to the end of to-space, and leave
   --  a forwarding pointer to the copy in its Left. Separate from Move so
   --  that its proof has a small context.

   procedure Scan_Cell
     (This  : in out Instance;
      Left  : Object;
      Right : Object)
     with Pre  => Collecting (This)
                  and then Counted (This)
                  and then This.Scan < This.Free
                  and then Is_Storable (This, Left)
                  and then Is_Storable (This, Right),
          Post => Same_Layout (This, This.Top'Old, This.To_Space'Old,
                               This.From_Space'Old)
                  and then Collecting (This)
                  and then Counted (This)
                  and then This.Scan = This.Scan'Old + 1
                  and then This.Free = This.Free'Old
                  and then This.From_Free = This.From_Free'Old;
   --  Store the moved contents of the cell at Scan and move Scan on. A
   --  separate procedure, like Forward_Copy, for a small proof context.

   procedure Move
     (This : in out Instance;
      Item : in out Object)
     with Pre  => Collecting (This)
                  and then Counted (This)
                  and then Is_Unmoved (This, Item),
          Post => Same_Layout (This, This.Top'Old, This.To_Space'Old,
                               This.From_Space'Old)
                  and then Collecting (This)
                  and then Counted (This)
                  and then Is_Storable (This, Item)
                  and then This.Scan = This.Scan'Old
                  and then This.Free >= This.Free'Old
                  and then This.From_Free = This.From_Free'Old;
   --  If Item is in the old heap, replace it by its to-space copy, copying
   --  its cell first if that has not happened yet.

   --------------
   -- After_GC --
   --------------

   procedure After_GC (This : in out Instance) is
   begin
      This.Stats.Reclaimed :=
        This.Stats.Reclaimed + Counter (This.Top - This.Free);
      This.Stats.Static_Top := This.Free;
      pragma Warnings
        (GNATprove, Off, "statement has no effect",
         Reason => "Poison_Dead_Space is a debugging switch, off by default");
      pragma Warnings
        (GNATprove, Off, "this statement is never reached",
         Reason => "Poison_Dead_Space is a debugging switch, off by default");
      if Poison_Dead_Space then
         --  The from-space is dead; poison it so any surviving stale pointer
         --  into it is caught rather than followed.
         Poison_From_Space (This);
      end if;
      pragma Warnings (GNATprove, On, "statement has no effect");
      pragma Warnings (GNATprove, On, "this statement is never reached");
      --  The run-time half of the Post: proof has it from Collecting.
      pragma Assert (Heap_Valid (This));
   end After_GC;

   ------------
   -- Append --
   ------------

   function Append
     (This   : in out Instance;
      Left   : Object;
      Right  : Object)
      return Object
   is
   begin
      pragma Assert
        (not Is_Full (This)
         and then Is_Storable (This, Left)
         and then Is_Storable (This, Right));
      This.Core (This.Free) := (Left, Right);
      This.Free := @ + 1;
      This.Stats.Alloc_Count := @ + 1;
      return Application (This.Free - 1);
   end Append;

   ---------------
   -- Before_GC --
   ---------------

   procedure Before_GC (This : in out Instance) is
   begin
      pragma Assert (Valid (This));
      Flip (This);
      --  Nothing is forwarded yet: every old-heap value is still in the
      --  old heap, inside from-space, or not an application.
      Lemma_None_Forwarded
        (This.Core, This.From_Space, This.From_Free, This.To_Space, This.Top);
      if This.Stats.Epoch_Remembered > This.Stats.Max_Remembered then
         This.Stats.Max_Remembered := This.Stats.Epoch_Remembered;
      end if;
      This.Stats.Epoch_Remembered := 0;
      This.Stats.Copied := 0;
      This.Stats.Static_Copied := 0;
      This.Stats.Transient_Copied := 0;
   end Before_GC;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (This        : in out Instance;
      Address     : Cell_Address;
      New_Address : out Cell_Address)
   is
   begin
      New_Address := This.Free;
      This.Core (This.Free) := This.Core (Address);
      This.Free := This.Free + 1;
   end Copy;

   ----------
   -- Flip --
   ----------

   procedure Flip (This : in out Instance) is
      Original_To_Space : constant Cell_Address := This.To_Space;
   begin
      pragma Assert
        (Natural (This.From_Space) + Natural (This.Space_Size)
           <= Natural (This.Last) + 1);
      This.From_Free := This.Free;
      This.To_Space := This.From_Space;
      This.From_Space := Original_To_Space;
      This.Top := This.To_Space + This.Space_Size;
      This.Free := This.To_Space;
      This.Scan := This.To_Space;
   end Flip;

   ------------------
   -- Forward_Copy --
   ------------------

   procedure Forward_Copy
     (This        : in out Instance;
      Address     : Cell_Address;
      New_Address : out Cell_Address)
   is
   begin
      --  By Collecting, the Left of an old-heap cell is unmoved or live;
      --  not being in to-space, it is unmoved, as Copy requires.
      pragma Assert (Is_Unmoved (This, This.Core (Address).Left));
      --  Address is in the old heap, inside from-space, so neither
      --  Copy, which writes to-space, nor any to-space write after it
      --  touches Address; and the write to Address below leaves every
      --  to-space cell as it was.
      pragma Assert
        (Address < This.To_Space or else Address >= This.Top);
      declare
         Before_Copy : constant Cell_Array := This.Core with Ghost;
      begin
         Copy (This, Address, New_Address);
         --  The copy went to to-space, outside the old heap.
         pragma Assert
           (if This.To_Space = 0
            then New_Address < This.From_Space
            else New_Address >= This.From_Free);
         declare
            pragma Assertion_Policy (Assert => Ignore);
         begin
            pragma Assert (New_Address /= Address);
            pragma Assert
              (not Is_Forwarded_Cell
                     (This.Core, Address, This.To_Space, This.Top));
            pragma Assert
              (for all A in This.Core'Range =>
                 (if A >= This.From_Space and then A < This.From_Free
                  then Before_Copy (A).Left = This.Core (A).Left));
         end;
         Lemma_Same_Lefts
           (Before_Copy, This.Core, This.From_Space, This.From_Free,
            This.To_Space, This.Top);
      end;
      declare
         Before_Forward : constant Cell_Array := This.Core with Ghost;
      begin
         This.Core (Address).Left := Application (New_Address);
         declare
            pragma Assertion_Policy (Assert => Ignore);
         begin
            pragma Assert
              (not Is_Forwarded_Cell
                     (Before_Forward, Address, This.To_Space, This.Top));
            pragma Assert
              (Is_Forwarded_Cell
                 (This.Core, Address, This.To_Space, This.Top));
            pragma Assert
              (for all A in This.Core'Range =>
                 (if A >= This.From_Space and then A < This.From_Free
                    and then A /= Address
                  then Before_Forward (A).Left = This.Core (A).Left));
         end;
         Lemma_Forward_One
           (Before_Forward, This.Core, This.From_Space, This.From_Free,
            This.To_Space, This.Top, Address);
      end;
   end Forward_Copy;

   --------
   -- GC --
   --------

   procedure GC (This : in out Instance) is
      --  Collecting walks both spaces; checked on every iteration it would
      --  make the loop quadratic. For proof only.
      pragma Assertion_Policy (Loop_Invariant => Ignore);
   begin
      while This.Scan < This.Free loop
         pragma Loop_Invariant
           (Collecting (This)
            and then Counted (This)
            and then This.Scan < This.Free);
         declare
            --  Copies, not a renaming of the cell: Move updates This, and
            --  a name for part of This held across that would alias it.
            Left  : Object := This.Core (This.Scan).Left;
            Right : Object := This.Core (This.Scan).Right;
         begin
            Move (This, Left);
            Move (This, Right);
            Scan_Cell (This, Left, Right);
         end;
      end loop;
   end GC;

   -----------------------
   -- Lemma_Forward_One --
   -----------------------

   procedure Lemma_Forward_One
     (Before   : Cell_Array;
      After    : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address;
      Address  : Cell_Address)
   is
   begin
      if Address = Upto - 1 then
         Lemma_Same_Lefts (Before, After, From, Upto - 1, To_Space, Top);
         pragma Assert
           (Count_Forwarded (Before, From, Upto, To_Space, Top)
              = Count_Forwarded (Before, From, Upto - 1, To_Space, Top));
         pragma Assert
           (Count_Forwarded (After, From, Upto, To_Space, Top)
              = Count_Forwarded (After, From, Upto - 1, To_Space, Top) + 1);
      else
         pragma Assert (Before (Upto - 1).Left = After (Upto - 1).Left);
         pragma Assert
           (Is_Forwarded_Cell (Before, Upto - 1, To_Space, Top)
              = Is_Forwarded_Cell (After, Upto - 1, To_Space, Top));
         Lemma_Forward_One
           (Before, After, From, Upto - 1, To_Space, Top, Address);
         pragma Assert
           (Count_Forwarded (Before, From, Upto, To_Space, Top)
              = Count_Forwarded (Before, From, Upto - 1, To_Space, Top)
                + (if Is_Forwarded_Cell (Before, Upto - 1, To_Space, Top)
                   then 1 else 0));
         pragma Assert
           (Count_Forwarded (After, From, Upto, To_Space, Top)
              = Count_Forwarded (After, From, Upto - 1, To_Space, Top)
                + (if Is_Forwarded_Cell (After, Upto - 1, To_Space, Top)
                   then 1 else 0));
      end if;
   end Lemma_Forward_One;

   --------------------------
   -- Lemma_None_Forwarded --
   --------------------------

   procedure Lemma_None_Forwarded
     (Core     : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address)
   is
   begin
      if Upto > From then
         pragma Assert
           (not Is_Forwarded_Cell (Core, Upto - 1, To_Space, Top));
         Lemma_None_Forwarded (Core, From, Upto - 1, To_Space, Top);
      end if;
   end Lemma_None_Forwarded;

   ----------------
   -- Lemma_Room --
   ----------------

   procedure Lemma_Room (This : Instance) is
   begin
      pragma Assert
        (Natural (This.From_Free - This.From_Space)
           <= Natural (This.Space_Size));
      pragma Assert
        (Natural (This.Free - This.To_Space) < Natural (This.Space_Size));
      pragma Assert
        (Natural (This.Free)
           < Natural (This.To_Space) + Natural (This.Space_Size));
   end Lemma_Room;

   ----------------------
   -- Lemma_Same_Lefts --
   ----------------------

   procedure Lemma_Same_Lefts
     (Before   : Cell_Array;
      After    : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address)
   is
   begin
      if Upto > From then
         pragma Assert (Before (Upto - 1).Left = After (Upto - 1).Left);
         Lemma_Same_Lefts (Before, After, From, Upto - 1, To_Space, Top);
      end if;
   end Lemma_Same_Lefts;

   -----------------------------
   -- Lemma_Unforwarded_Bound --
   -----------------------------

   procedure Lemma_Unforwarded_Bound
     (Core     : Cell_Array;
      From     : Cell_Address;
      Upto     : Cell_Address;
      To_Space : Cell_Address;
      Top      : Cell_Address;
      Address  : Cell_Address)
   is
   begin
      --  Where Address is the last cell, the Post of Count_Forwarded on
      --  the rest of the range is enough.
      if Address /= Upto - 1 then
         Lemma_Unforwarded_Bound
           (Core, From, Upto - 1, To_Space, Top, Address);
      end if;
   end Lemma_Unforwarded_Bound;

   ---------------
   -- Live_Cell --
   ---------------

   procedure Live_Cell
     (This  : Instance;
      Index : Natural;
      Left  : out Object;
      Right : out Object)
   is
      Address : constant Cell_Address :=
                  This.To_Space + Cell_Address (Index);
   begin
      Left  := This.Core (Address).Left;
      Right := This.Core (Address).Right;
   end Live_Cell;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Instance) is
      Space_Size  : constant Cell_Address := (This.Last + 1) / 2;
      To_Space    : constant Cell_Address := 0;
      From_Space  : constant Cell_Address := Space_Size;
   begin
      This.Top        := To_Space + Space_Size;
      This.Free       := To_Space;
      This.From_Space := From_Space;
      This.To_Space   := To_Space;
      This.Space_Size := Space_Size;
      This.Scan       := To_Space;
      This.From_Free  := From_Space;
      --  Steps for the prover: halving cannot overflow, and two halves
      --  never exceed the whole; then Valid, conjunct by conjunct.
      pragma Assert (Natural (Space_Size) >= 1);
      pragma Assert
        (Natural (Space_Size) + Natural (Space_Size)
           <= Natural (This.Last) + 1);
      pragma Assert (This.Top = This.To_Space + This.Space_Size);
      pragma Assert (Natural (This.Top) <= Natural (This.Last) + 1);
      pragma Assert
        (Natural (This.From_Space) + Natural (This.Space_Size)
           <= Natural (This.Last) + 1);
      pragma Assert
        (This.From_Space <= This.From_Free
         and then Natural (This.From_Free)
                    <= Natural (This.From_Space) + Natural (This.Space_Size));
   end Initialize;

   ----------
   -- Left --
   ----------

   function Left
     (This : Instance;
      App  : Object)
      return Object
   is
   begin
      pragma Assert (Is_Live (This, App));
      return This.Core (Payload (App)).Left;
   end Left;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (This : in out Instance;
      Root : in out Object)
   is
   begin
      pragma Assert (Is_Unmoved (This, Root), "stale root");
      Move (This, Root);
      pragma Assert (Is_Storable (This, Root));
   end Mark;

   ----------
   -- Move --
   ----------

   procedure Move
     (This : in out Instance;
      Item : in out Object)
   is
   begin
      if not In_From_Space (This, Item) then
         --  The old heap lies inside from-space, so an unmoved value
         --  outside from-space is not an application at all.
         pragma Assert (not Is_Application (Item));
         return;
      end if;

      declare
         Address     : constant Cell_Address := Payload (Item);
         New_Address : Cell_Address;
      begin
         --  A from-space cell whose Left points into to-space has already
         --  been copied, and its Left is the forwarding address.
         if not In_To_Space (This, This.Core (Address).Left) then
            This.Stats.Copied := @ + 1;
            if Address < This.Stats.Static_Top then
               This.Stats.Static_Copied := @ + 1;
            else
               This.Stats.Transient_Copied := @ + 1;
            end if;
            --  This cell is not forwarded, so fewer than all old-heap cells
            --  are; by Counted, fewer cells than that have been copied, and
            --  the old heap is no bigger than a semispace. So there is room.
            Lemma_Unforwarded_Bound
              (This.Core, This.From_Space, This.From_Free,
               This.To_Space, This.Top, Address);
            Lemma_Room (This);
            pragma Assert (This.Free < This.Top);
            Forward_Copy (This, Address, New_Address);
            Item := Application (New_Address);
         else
            --  Forwarded already: by Collecting the Left of an old-heap
            --  cell is unmoved or live, and an unmoved value is never in
            --  to-space, so it is live.
            pragma Assert (In_Old_Heap (This, Item));
            pragma Assert
              (Is_Unmoved (This, This.Core (Address).Left)
               or else Is_Live (This, This.Core (Address).Left));
            pragma Assert (not In_Old_Heap (This, This.Core (Address).Left));
            pragma Assert (Is_Live (This, This.Core (Address).Left));
            Item := This.Core (Address).Left;
         end if;
      end;
   end Move;

   -----------------------
   -- Poison_From_Space --
   -----------------------

   procedure Poison_From_Space (This : in out Instance) is
   begin
      for Address in This.From_Space
                       .. This.From_Space + This.Space_Size - 1
      loop
         This.Core (Address) := (Invalid, Invalid);
      end loop;
   end Poison_From_Space;

   -----------
   -- Right --
   -----------

   function Right
     (This : Instance;
      App  : Object)
      return Object
   is
   begin
      pragma Assert (Is_Live (This, App));
      return This.Core (Payload (App)).Right;
   end Right;

   ---------------
   -- Scan_Cell --
   ---------------

   procedure Scan_Cell
     (This  : in out Instance;
      Left  : Object;
      Right : Object)
   is
      Before_Write : constant Cell_Array := This.Core with Ghost;
   begin
      --  Scan is in to-space and the old heap is in from-space, so the
      --  write below leaves every old-heap cell as it was.
      pragma Assert
        (This.Scan >= This.To_Space and then This.Scan < This.Top);
      pragma Assert
        (if This.To_Space = 0
         then This.Scan < This.From_Space
         else This.Scan >= This.From_Free);
      This.Core (This.Scan) := (Left, Right);
      This.Scan := @ + 1;
      declare
         --  Proof-only hints: they mention ghost values, which are not
         --  compiled.
         pragma Assertion_Policy (Assert => Ignore);
      begin
         pragma Assert
           (for all A in This.Core'Range =>
              (if A /= This.Scan - 1 then This.Core (A) = Before_Write (A)));
         pragma Assert
           (for all A in This.Core'Range =>
              (if A >= This.From_Space and then A < This.From_Free
               then Before_Write (A).Left = This.Core (A).Left));
      end;
      Lemma_Same_Lefts
        (Before_Write, This.Core, This.From_Space, This.From_Free,
         This.To_Space, This.Top);
   end Scan_Cell;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object)
   is
   begin
      pragma Assert (Is_Live (This, App) and then Is_Storable (This, To));
      if Payload (App) < This.Stats.Static_Top
        and then Is_Application (To)
        and then Payload (To) >= This.Stats.Static_Top
      then
         This.Stats.Remembered_Writes := @ + 1;
         This.Stats.Epoch_Remembered  := @ + 1;
      end if;
      This.Core (Payload (App)).Left := To;
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object)
   is
   begin
      pragma Assert (Is_Live (This, App) and then Is_Storable (This, To));
      if Payload (App) < This.Stats.Static_Top
        and then Is_Application (To)
        and then Payload (To) >= This.Stats.Static_Top
      then
         This.Stats.Remembered_Writes := @ + 1;
         This.Stats.Epoch_Remembered  := @ + 1;
      end if;
      This.Core (Payload (App)).Right := To;
   end Set_Right;

end Skit.Memory;
