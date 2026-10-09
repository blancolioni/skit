package body Skit.Memory is

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

   procedure Poison_From_Space (This : in out Instance);

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

   function Copy
     (This    : in out Instance;
      Address : Cell_Address)
      return Cell_Address
     with Pre  => This.Free < This.Top
                  and then In_From_Space (This, Application (Address)),
          Post => Copy'Result = This.Free'Old
                  and then This.Free = This.Free'Old + 1;
   --  The live set has to fit in one semispace; the Pre is where that
   --  assumption is finally stated.

   function Move
     (This : in out Instance;
      Item : Object)
      return Object;

   procedure Flip (This : in out Instance)
     with Post => This.To_Space = This.From_Space'Old
                  and then This.From_Space = This.To_Space'Old
                  and then This.Free = This.To_Space
                  and then This.Scan = This.To_Space;

   --------------
   -- After_GC --
   --------------

   procedure After_GC (This : in out Instance) is
   begin
      This.Reclaimed := This.Reclaimed
        + (Natural (This.Top) - Natural (This.Free));
      This.Static_Top := This.Free;
      if Poison_Dead_Space then
         --  The from-space is dead; poison it so any surviving stale pointer
         --  into it is caught rather than followed.
         Poison_From_Space (This);
      end if;
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
      This.Alloc_Count := @ + 1;
      return Application (This.Free - 1);
   end Append;

   ---------------
   -- Before_GC --
   ---------------

   procedure Before_GC (This : in out Instance) is
   begin
      Flip (This);
      if This.Epoch_Remembered > This.Max_Remembered then
         This.Max_Remembered := This.Epoch_Remembered;
      end if;
      This.Epoch_Remembered := 0;
      This.Copied := 0;
      This.Static_Copied := 0;
      This.Transient_Copied := 0;
   end Before_GC;

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
         pragma Loop_Invariant
           (Valid (This) and then This.Scan < This.Free);
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
         Address : constant Cell_Address := Payload (Item);
         Cell    : Cell_Type renames This.Core (Address);
      begin
         if not In_To_Space (This, Cell.Left) then
            This.Copied := This.Copied + 1;
            if Address < This.Static_Top then
               This.Static_Copied := @ + 1;
            else
               This.Transient_Copied := @ + 1;
            end if;
            Cell.Left := Application (Copy (This, Address));
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
      if Payload (App) < This.Static_Top
        and then Is_Application (To)
        and then Payload (To) >= This.Static_Top
      then
         This.Remembered_Writes := @ + 1;
         This.Epoch_Remembered  := @ + 1;
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
      if Payload (App) < This.Static_Top
        and then Is_Application (To)
        and then Payload (To) >= This.Static_Top
      then
         This.Remembered_Writes := @ + 1;
         This.Epoch_Remembered  := @ + 1;
      end if;
      This.Core (Payload (App)).Right := To;
   end Set_Right;

end Skit.Memory;
