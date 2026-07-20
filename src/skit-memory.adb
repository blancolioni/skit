package body Skit.Memory is

   --  Compile-time gate for the GC self-checks below. When False (the
   --  default), the constant folds and every guarded block is
   --  dead-code-eliminated, so release builds pay nothing. Flip to True to
   --  run heap-integrity validation around each collection and to poison the
   --  dead from-space -- this caught the cached-pointer bug in ADR 0008.
   Heap_Checks : constant Boolean := False;

   --  Poison value written over every dead from-space cell after a collection.
   --  An application pointer to the top of the address space: it lands outside
   --  any live semispace, so a stale read of a reclaimed cell either trips the
   --  heap check or faults on the out-of-range index instead of silently
   --  returning plausible-looking garbage.
   Invalid : constant Object := (Object_Payload'Last, Application_Object);

   procedure Check_Heap
     (This  : Instance;
      Where : String);

   procedure Poison_From_Space (This : in out Instance);

   function In_From_Space
     (This   : Instance;
      Item   : Object)
      return Boolean
   is (Item.Tag = Application_Object
         and then Item.Payload in
           This.From_Space .. This.From_Space + This.Space_Size - 1);

   function In_To_Space
     (This   : Instance;
      Item   : Object)
      return Boolean
   is (Item.Tag = Application_Object
       and then Item.Payload in
         This.To_Space .. This.To_Space + This.Space_Size - 1);

   function Copy
     (This    : in out Instance;
      Address : Cell_Address)
      return Cell_Address;

   function Move
     (This : in out Instance;
      Item : Object)
      return Object;

   procedure Flip (This : in out Instance);

   --------------
   -- After_GC --
   --------------

   procedure After_GC (This : in out Instance) is
   begin
      This.Reclaimed := This.Reclaimed
        + (Natural (This.Top) - Natural (This.Free));
      This.Static_Top := This.Free;
      if Heap_Checks then
         --  Every live cell now lives in to-space and points only within it.
         Check_Heap (This, "after GC");
         --  The from-space is dead; poison it so any surviving stale pointer
         --  into it is caught on the next collection rather than followed.
         Poison_From_Space (This);
      end if;
   end After_GC;

   ------------
   -- Append --
   ------------

   procedure Append
     (This    : in out Instance;
      Left    : Object;
      Right   : Object;
      New_App : out Object)
   is
   begin
      This.Core (This.Free) := (Left, Right);
      This.Free := @ + 1;
      This.Alloc_Count := @ + 1;
      New_App := (This.Free - 1, Application_Object);
   end Append;

   ---------------
   -- Before_GC --
   ---------------

   procedure Before_GC (This : in out Instance) is
   begin
      if Heap_Checks then
         --  Validate the live set before the flip, while it is still the
         --  to-space; a corrupt pointer here predates this collection.
         Check_Heap (This, "before GC");
      end if;
      Flip (This);
      if This.Epoch_Remembered > This.Max_Remembered then
         This.Max_Remembered := This.Epoch_Remembered;
      end if;
      This.Epoch_Remembered := 0;
      This.Copied := 0;
      This.Static_Copied := 0;
      This.Transient_Copied := 0;
   end Before_GC;

   ----------------
   -- Check_Heap --
   ----------------

   procedure Check_Heap
     (This  : Instance;
      Where : String)
   is
      Low  : constant Cell_Address := This.To_Space;
      High : constant Cell_Address := This.Free;

      procedure Check_Child
        (Field   : String;
         Address : Cell_Address;
         Child   : Object);

      procedure Check_Child
        (Field   : String;
         Address : Cell_Address;
         Child   : Object)
      is
      begin
         if Is_Application (Child)
           and then Child.Payload not in Low .. High - 1
         then
            raise Program_Error with
              "heap check (" & Where & "): " & Field
              & " of cell" & Cell_Address'Image (Address)
              & " points to" & Cell_Address'Image (Child.Payload)
              & " outside to-space [" & Cell_Address'Image (Low)
              & " .." & Cell_Address'Image (High) & ")";
         end if;
      end Check_Child;

   begin
      --  Guard: Cell_Address is modular, so an empty live set (High = Low)
      --  would make High - 1 wrap to the top of the address space.
      if High = Low then
         return;
      end if;
      for Address in Low .. High - 1 loop
         declare
            Cell : Cell_Type renames This.Core (Address);
         begin
            Check_Child ("left", Address, Cell.Left);
            Check_Child ("right", Address, Cell.Right);
         end;
      end loop;
   end Check_Heap;

   ----------
   -- Copy --
   ----------

   function Copy
     (This    : in out Instance;
      Address : Cell_Address)
      return Cell_Address
   is
   begin
      return Result : constant Cell_Address := This.Free do
         This.Core (This.Free) := This.Core (Address);
         This.Free := This.Free + 1;
      end return;
   end Copy;

   ----------
   -- Flip --
   ----------

   procedure Flip (This : in out Instance) is
      Original_To_Space : constant Cell_Address := This.To_Space;
   begin
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
   begin
      while This.Scan < This.Free loop
         declare
            Cell      : Cell_Type renames This.Core (This.Scan);
            New_Left  : constant Object := Move (This, Cell.Left);
            New_Right : constant Object := Move (This, Cell.Right);
         begin
            Cell := (New_Left, New_Right);
            This.Scan := @ + 1;
         end;
      end loop;
   end GC;

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
      This.Scan       := To_Space;
      This.From_Space := From_Space;
      This.To_Space   := To_Space;
      This.Space_Size := Space_Size;
   end Initialize;

   -------------
   -- Is_Full --
   -------------

   function Is_Full
     (This : Instance)
      return Boolean
   is
   begin
      return This.Free = This.Top;
   end Is_Full;

   ----------
   -- Left --
   ----------

   function Left
     (This : Instance;
      App  : Object)
      return Object
   is
   begin
      return This.Core (App.Payload).Left;
   end Left;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (This : in out Instance;
      Root : in out Object)
   is
   begin
      Root := Move (This, Root);
   end Mark;

   ----------
   -- Move --
   ----------

   function Move
     (This : in out Instance;
      Item : Object)
      return Object
   is
   begin
      if not In_From_Space (This, Item) then
         return Item;
      end if;

      declare
         Address : constant Cell_Address := Item.Payload;
         Cell    : Cell_Type renames This.Core (Address);
      begin
         if not In_To_Space (This, Cell.Left) then
            This.Copied := This.Copied + 1;
            if Address < This.Static_Top then
               This.Static_Copied := @ + 1;
            else
               This.Transient_Copied := @ + 1;
            end if;
            Cell.Left :=
              (Copy (This, Address), Application_Object);
         end if;
         return Cell.Left;
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
      return This.Core (App.Payload).Right;
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
      if App.Payload < This.Static_Top
        and then Is_Application (To)
        and then To.Payload >= This.Static_Top
      then
         This.Remembered_Writes := @ + 1;
         This.Epoch_Remembered  := @ + 1;
      end if;
      This.Core (App.Payload).Left := To;
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
      if App.Payload < This.Static_Top
        and then Is_Application (To)
        and then To.Payload >= This.Static_Top
      then
         This.Remembered_Writes := @ + 1;
         This.Epoch_Remembered  := @ + 1;
      end if;
      This.Core (App.Payload).Right := To;
   end Set_Right;

end Skit.Memory;
