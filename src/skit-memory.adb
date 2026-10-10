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
   Invalid : constant Object := Application (Object_Payload'Last);

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
       and then This.From_Space = From_Space)
     with Ghost;
   --  The semispaces have not moved. Stating it lets a prover rebuild
   --  Valid by equalities after Copy and Move, instead of from scratch.

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
                                  New_Address => This.Core'Old (Address));
   --  Copy an old-heap cell that has not been copied yet to the end of
   --  to-space. The live set has to fit in one semispace; see Move.

   procedure Move
     (This : in out Instance;
      Item : in out Object)
     with Pre  => Collecting (This) and then Is_Unmoved (This, Item),
          Post => Same_Layout (This, This.Top'Old, This.To_Space'Old,
                               This.From_Space'Old)
                  and then Collecting (This)
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
      This.From_Free := This.Free;
      This.To_Space := This.From_Space;
      This.From_Space := Original_To_Space;
      This.Top := This.To_Space + This.Space_Size;
      This.Free := This.To_Space;
      This.Scan := This.To_Space;
   end Flip;

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
           (Collecting (This) and then This.Scan < This.Free);
         declare
            --  Copies, not a renaming of the cell: Move updates This, and
            --  a name for part of This held across that would alias it.
            Left  : Object := This.Core (This.Scan).Left;
            Right : Object := This.Core (This.Scan).Right;
         begin
            Move (This, Left);
            Move (This, Right);
            --  Scan is in to-space and the old heap is in from-space, so
            --  the write below leaves every old-heap cell as it was.
            pragma Assert
              (This.Scan >= This.To_Space and then This.Scan < This.Top);
            pragma Assert
              (if This.To_Space = 0
               then This.Scan < This.From_Space
               else This.Scan >= This.From_Free);
            This.Core (This.Scan) := (Left, Right);
            This.Scan := @ + 1;
         end;
      end loop;
   end GC;

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
            --  Each from-space cell is copied at most once (it is marked
            --  as forwarded straight after), and from-space holds
            --  Space_Size cells, so copying never runs past Top. Proving
            --  that needs a count of forwarded cells; until then it is an
            --  assumption, recorded in proof/README.md.
            pragma Assume
              (This.Free < This.Top,
               "a collection copies each from-space cell at most once");
            Copy (This, Address, New_Address);
            --  Address is in the old heap, inside from-space, so the write
            --  below leaves every to-space cell as it was.
            pragma Assert
              (Address < This.To_Space or else Address >= This.Top);
            This.Core (Address).Left := Application (New_Address);
            pragma Assert (Valid (This));
            pragma Assert (Is_Live (This, This.Core (Address).Left));
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
         end if;
         Item := This.Core (Address).Left;
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
