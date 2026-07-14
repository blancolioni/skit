with Ada.Text_IO;
with Skit.Debug;

package body Skit.Memory is

   Check_Heap_Enabled : constant Boolean := True;

   procedure Remember
     (This : in out Instance;
      App  : Object;
      To   : Object)
     with Inline_Always;

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

   procedure Check_Heap (This : Instance);

   --------------
   -- After_GC --
   --------------

   procedure After_GC (This : in out Instance) is
   begin
      This.Reclaimed := This.Reclaimed
        + (Natural (This.Top) - Natural (This.Free));
      if Check_Heap_Enabled then
         Check_Heap (This);
         This.Core (This.From_Space .. This.From_Space + This.Space_Size - 1)
           := [others => (Invalid, Invalid)];
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
      This.Core (This.Free) := (Left, Right);
      This.Free := @ + 1;
      This.Alloc_Count := @ + 1;
      return (This.Free - 1, Application_Object);
   end Append;

   -------------------
   -- Append_Static --
   -------------------

   function Append_Static
     (This   : in out Instance;
      Left   : Object;
      Right  : Object)
      return Object
   is
   begin
      This.Core (This.Static_Free) := (Left, Right);
      This.Static_Free := @ + 1;
      return (This.Static_Free - 1, Application_Object);
   end Append_Static;

   ---------------
   -- Before_GC --
   ---------------

   procedure Before_GC (This : in out Instance) is

      function Points_Young (O : Object) return Boolean
      is (Is_Application (O)
          and then O.Payload in
            This.To_Space .. This.To_Space + This.Space_Size - 1);

      function In_Remembered (A : Cell_Address) return Boolean
      is (for some Addr of This.Remembered (1 .. This.Remembered_Count) =>
             Addr = A);

   begin

      if False and then Check_Heap_Enabled then
         for A in 0 .. This.Static_Top - 1 loop
            if Points_Young (This.Core (A).Left)
              or else Points_Young (This.Core (A).Right)
            then
               pragma Assert
                 (This.Remembered_Full
                  or else In_Remembered (A),
                  "address" & A'Image & ": "
                  & "(" & Debug.Image (This.Core (A).Left)
                  & " "
                  & Debug.Image (This.Core (A).Right)
                  & ")"
                  & " young pointer but not remembered");
            end if;
         end loop;
      end if;

      Flip (This);

      if This.Epoch_Remembered > This.Max_Remembered then
         This.Max_Remembered := This.Epoch_Remembered;
      end if;

      This.Epoch_Remembered := 0;
      This.Copied := 0;
      This.Static_Copied := 0;
      This.Transient_Copied := 0;

      if True or else This.Remembered_Full then
         --  fallback: whole static region.  Rebuild the compact remembered
         --  set from cells that still point young after forwarding, so we can
         --  leave full mode instead of scanning-all on every future GC.
         declare
            New_Count : Natural := 0;
            Overflow  : Boolean := False;
         begin
            for A in 0 .. This.Static_Top - 1 loop
               This.Core (A).Left  := Move (This, This.Core (A).Left);
               This.Core (A).Right := Move (This, This.Core (A).Right);
               if Points_Young (This.Core (A).Left)
                 or else Points_Young (This.Core (A).Right)
               then
                  if New_Count < Remembered_Max then
                     New_Count := @ + 1;
                     This.Remembered (New_Count) := A;
                  else
                     Overflow := True;
                  end if;
               end if;
            end loop;
            This.Remembered_Count := (if Overflow then 0 else New_Count);
            This.Remembered_Full  := Overflow;
         end;
      else
         --  Forward each remembered root, then keep only the cells that still
         --  point young.  Copying survivors stay in the (new) young to-space,
         --  so a forwarded cell is normally still an old->young pointer and
         --  must remain remembered.  Cells since overwritten to point static
         --  are dropped; adding each address once also collapses the
         --  intra-epoch duplicates left by the last-entry-only barrier dedup.
         declare
            Old_Count : constant Natural := This.Remembered_Count;
            New_Count : Natural := 0;
         begin
            for I in 1 .. Old_Count loop
               declare
                  A : constant Cell_Address := This.Remembered (I);
               begin
                  This.Core (A).Left  := Move (This, This.Core (A).Left);
                  This.Core (A).Right := Move (This, This.Core (A).Right);
                  if Points_Young (This.Core (A).Left)
                    or else Points_Young (This.Core (A).Right)
                  then
                     New_Count := @ + 1;
                     This.Remembered (New_Count) := A;
                  end if;
               end;
            end loop;
            This.Remembered_Count := New_Count;
         end;
         This.Remembered_Full := False;
      end if;
   end Before_GC;

   ----------------
   -- Check_Heap --
   ----------------

   procedure Check_Heap (This : Instance) is

      Checked : array (Cell_Address range 0 .. This.Last) of Boolean :=
                  [others => False];

      procedure Assert_Valid
        (A    : Cell_Address;
         Cell : Cell_Type);

      ------------------
      -- Assert_Valid --
      ------------------

      procedure Assert_Valid
        (A    : Cell_Address;
         Cell : Cell_Type)
      is
      begin
         if Checked (A) then
            return;
         end if;

         pragma Assert
           (Valid (This, Cell.Left),
            "address" & A'Image & ": left "
            & Skit.Debug.Image (Cell.Left)
            & " invalid");
         pragma Assert
           (Valid (This, Cell.Right),
            "address" & A'Image & ": right "
            & Debug.Image (Cell.Right)
            & " invalid");

         Checked (A) := True;
      end Assert_Valid;

   begin
      pragma Assert (This.Scan = This.Free);
      pragma Assert (This.Static_Top <= This.Free
                     and then This.Free <= This.Top);
      for A in 0 .. This.Static_Top - 1 loop
         Assert_Valid (A, This.Core (A));
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
      Static_Size : constant Cell_Address := (This.Last + 1) / 2;
      Space_Size  : constant Cell_Address := (This.Last + 1) / 4;
      To_Space    : constant Cell_Address := Static_Size;
      From_Space  : constant Cell_Address := Static_Size + Space_Size;
   begin
      This.Static_Space := 0;
      This.Static_Top   := Static_Size;
      This.Static_Free  := This.Static_Space;
      This.Top          := To_Space + Space_Size;
      This.Free         := To_Space;
      This.From_Space   := From_Space;
      This.To_Space     := To_Space;
      This.Space_Size   := Space_Size;
      This.Core         := [others => (Invalid, Invalid)];
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

   --------------------
   -- Is_Static_Full --
   --------------------

   function Is_Static_Full
     (This : Instance)
      return Boolean
   is
   begin
      return This.Static_Free = This.Static_Top;
   end Is_Static_Full;

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

   --------------
   -- Remember --
   --------------

   procedure Remember
     (This : in out Instance;
      App  : Object;
      To   : Object)
   is
   begin
      if App.Payload < This.Static_Top
        and then Is_Application (To)
        and then To.Payload >= This.Static_Top
      then
         Ada.Text_IO.Put_Line
           ("remember:" & App.Payload'Image
            & " ->" & To.Payload'Image);
         if not This.Remembered_Full
           and then (This.Remembered_Count = 0
                     or else This.Remembered (This.Remembered_Count)
                     /= App.Payload)
         then
            This.Remembered_Writes := @ + 1;
            This.Epoch_Remembered  := @ + 1;
            if This.Remembered_Count < Remembered_Max then
               This.Remembered_Count := @ + 1;
               This.Remembered (This.Remembered_Count) := App.Payload;
            else
               --  degrade to scan-all, still correct
               This.Remembered_Full := True;
            end if;
         end if;
      end if;
   end Remember;

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
      Remember (This, App, To);
      This.Core (App.Payload).Left := To;
      if App.Payload = 204 then
         Ada.Text_IO.Put_Line
           ("cell[" & App.Payload'Image & " ].Left := "
            & Debug.Image (To));
      end if;
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
      Remember (This, App, To);
      This.Core (App.Payload).Right := To;
      if App.Payload = 204 then
         Ada.Text_IO.Put_Line
           ("cell[" & App.Payload'Image & " ].Right := "
            & Debug.Image (To));
      end if;
   end Set_Right;

end Skit.Memory;
