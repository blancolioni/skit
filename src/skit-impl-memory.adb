with Ada.Calendar;
with Ada.Text_IO;

with Skit.Exceptions;

package body Skit.Impl.Memory is

   function In_From_Space
     (This   : Instance;
      Item   : Object)
      return Boolean;

   function In_To_Space
     (This   : Instance;
      Item   : Object)
      return Boolean;

   function Copy
     (This    : in out Instance;
      Address : Cell_Address)
      return Cell_Address;

   function Move
     (This : in out Instance;
      Item : Object)
      return Object;

   procedure Flip (This : in out Instance);

   procedure Mark
     (This : in out Instance;
      Item : in out Object);

   procedure GC
     (This : in out Instance);

   -------------------
   -- Add_Container --
   -------------------

   procedure Add_Container
     (This      : in out Instance;
      Container : not null access Skit.Containers.Abstraction'Class)
   is
   begin
      This.Containers.Append (Container_Reference (Container));
   end Add_Container;

   -----------------------
   -- Free_And_Allocate --
   -----------------------

   function Free_And_Allocate
     (This   : in out Instance;
      Left   : in out Object;
      Right  : in out Object)
      return Object
   is
   begin
      This.Alloc_Left := Left;
      This.Alloc_Right := Right;

      GC (This);

      if This.Free = This.Top then
         raise Constraint_Error with "out of memory";
      end if;

      Left := This.Alloc_Left;
      Right := This.Alloc_Right;

      return Quick_Allocate (This, Left, Right);
   end Free_And_Allocate;

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

   ------------
   -- Create --
   ------------

   function Create
     (Core_Size  : Positive)
      return Reference
   is
      Top_Address : constant Cell_Address := Cell_Address (Core_Size - 1);
      Space_Size  : constant Cell_Address := Cell_Address (Core_Size / 2);
      To_Space    : constant Cell_Address := 0;
      From_Space  : constant Cell_Address := Space_Size;

      Mem : constant Reference :=
              new Instance'
                (Last              => Top_Address,
                 Core              => <>,
                 Top               => To_Space + Space_Size,
                 Free              => To_Space,
                 From_Space        => From_Space,
                 To_Space          => To_Space,
                 Space_Size        => Space_Size,
                 others            => <>);
   begin
      return Mem;
   end Create;

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

   procedure GC
     (This : in out Instance)
   is
      use Ada.Calendar;
      Start           : constant Time := Clock;
      Old_Alloc_Count : constant Natural := This.Alloc_Count;
   begin
      Flip (This);

      Mark (This, This.Alloc_Left);
      Mark (This, This.Alloc_Right);

      declare
         procedure Set (Item : in out Object);

         ---------
         -- Set --
         ---------

         procedure Set (Item : in out Object) is
         begin
            Mark (This, Item);
         end Set;

      begin
         for Container of This.Containers loop
            Container.Mark (Set'Access);
         end loop;
      end;

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

      This.Alloc_Count := Natural (This.Scan - This.To_Space);
      This.Max_Active_Cells := Natural'Max (@, This.Alloc_Count);
      This.Active_Cells := This.Alloc_Count;
      This.Reclaimed := @ + Old_Alloc_Count - This.Alloc_Count;
      This.GC_Time := @ + Clock - Start;
      This.GC_Count := @ + 1;

      if This.Trace_GC then
         Ada.Text_IO.Put
           ("[GC"
            & Integer'Image (-This.GC_Count)
            & Integer'Image (-This.Alloc_Count)
            & "]");
      end if;
   end GC;

   -------------------
   -- In_From_Space --
   -------------------

   function In_From_Space
     (This   : Instance;
      Item   : Object)
      return Boolean
   is
   begin
      return Item.Tag = Application_Object
        and then Item.Payload in
          This.From_Space .. This.From_Space + This.Space_Size - 1;
   end In_From_Space;

   -----------------
   -- In_To_Space --
   -----------------

   function In_To_Space
     (This   : Instance;
      Item   : Object)
      return Boolean
   is
   begin
      return Item.Tag = Application_Object
        and then Item.Payload in
          This.To_Space .. This.To_Space + This.Space_Size - 1;
   end In_To_Space;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (This : in out Instance;
      Item : in out Object)
   is
   begin
      Item := Move (This, Item);
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

   --------------------
   -- Quick_Allocate --
   --------------------

   function Quick_Allocate
     (This   : in out Instance;
      Left   : Object;
      Right  : Object)
      return Object
   is
      Result : constant Object := (This.Free, Application_Object);
   begin
      This.Core (This.Free) := (Left, Right);
      This.Free := This.Free + 1;
      This.Alloc_Count := This.Alloc_Count + 1;
      This.Total_Alloc_Count := @ + 1;
      return Result;
   end Quick_Allocate;

   ------------
   -- Report --
   ------------

   procedure Report (This : Instance) is
   begin
      Ada.Text_IO.Put_Line
        ("Total number of cells:"
         & Cell_Address'Image (This.Core'Length / 2));
      Ada.Text_IO.Put_Line
        ("Allocated cell count: "
         & Cell_Address'Image (This.Free - This.To_Space));
      Ada.Text_IO.Put_Line
        ("Free cell count: "
         & Cell_Address'Image (This.Top - This.Free));
      Ada.Text_IO.Put_Line
        ("Active cell count:"
         & This.Active_Cells'Image
         & "; max:"
         & This.Max_Active_Cells'Image);

      Ada.Text_IO.Put_Line
        ("GC:"
         & Natural'Image (This.GC_Count)
         & " @"
         & Natural'Image (Natural (This.GC_Time * 1000.0))
         & "ms");
      Ada.Text_IO.Put_Line
        ("Allocated cells:"
         & Natural'Image (This.Total_Alloc_Count));
      Ada.Text_IO.Put_Line
        ("Reclaimed cells:"
         & Natural'Image (This.Reclaimed));
   end Report;

   --------------
   -- Set_Left --
   --------------

   procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object)
   is
   begin
      if App.Payload > This.Last then
         raise Skit.Exceptions.Address_Error with App.Payload'Image;
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
      if App.Payload > This.Last then
         raise Skit.Exceptions.Address_Error with App.Payload'Image;
      end if;

      This.Core (App.Payload).Right := To;
   end Set_Right;

   --------------
   -- Trace_GC --
   --------------

   procedure Trace_GC
     (This    : in out Instance;
      Enabled : Boolean)
   is
   begin
      This.Trace_GC := Enabled;
   end Trace_GC;

end Skit.Impl.Memory;
