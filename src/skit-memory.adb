package body Skit.Memory is

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
      null;
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

   ---------------
   -- Before_GC --
   ---------------

   procedure Before_GC (This : in out Instance) is
   begin
      Flip (This);
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
            Cell.Left :=
              (Copy (This, Address), Application_Object);
         end if;
         return Cell.Left;
      end;

   end Move;

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
      This.Core (App.Payload).Right := To;
   end Set_Right;

end Skit.Memory;
