with Ada.Calendar;

with Skit.Exceptions;

package body Skit.Impl.Memory is

   subtype Cell_Address is Object_Payload;

   type Cell_Type is
      record
         Left, Right : Object;
      end record;

   type Cell_Array is array (Cell_Address range <>) of Cell_Type;

   type Marks_Reference is access all Skit.Marks.Abstraction'Class;

   subtype Parent is Skit.Allocator.Abstraction;
   type Instance (Last : Cell_Address) is new Parent with
      record
         Core        : Cell_Array (0 .. Last);
         Top         : Cell_Address;
         Free        : Cell_Address;
         From_Space  : Cell_Address;
         To_Space    : Cell_Address;
         Space_Size  : Cell_Address;
         Scan        : Cell_Address;
         Marks       : Marks_Reference;
         Alloc_Left  : Object;
         Alloc_Right : Object;
         Alloc_Count : Natural := 0;
         Reclaimed   : Natural := 0;
         GC_Time     : Duration := 0.0;
         GC_Count    : Natural := 0;
      end record;

   overriding function Allocate
     (This   : in out Instance;
      Left   : in out Object;
      Right  : in out Object)
      return Object;

   overriding function Left
     (This : Instance;
      App  : Object)
      return Object;

   overriding function Right
     (This : Instance;
      App  : Object)
      return Object;

   overriding procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object);

   overriding procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object);

   function In_From_Space
     (This   : Instance'Class;
      Item   : Object)
      return Boolean;

   function In_To_Space
     (This   : Instance'Class;
      Item   : Object)
      return Boolean;

   function Copy
     (This    : in out Instance'Class;
      Address : Cell_Address)
      return Cell_Address;

   function Move
     (This : in out Instance'Class;
      Item : Object)
      return Object;

   procedure Flip (This : in out Instance'Class);

   procedure Mark
     (This : in out Instance'Class;
      Item : in out Object);

   procedure GC
     (This : in out Instance'Class);

   --------------
   -- Allocate --
   --------------

   overriding function Allocate
     (This   : in out Instance;
      Left   : in out Object;
      Right  : in out Object)
      return Object
   is
   begin
      This.Alloc_Left := Left;
      This.Alloc_Right := Right;

      if This.Free = This.Top then
         This.GC;
         if This.Free = This.Top then
            raise Constraint_Error with "out of memory";
         end if;
      end if;

      return Result : constant Object :=
        (This.Free, Application_Object)
      do
         This.Core (This.Free) := (This.Alloc_Left, This.Alloc_Right);
         This.Free := This.Free + 1;
         This.Alloc_Count := This.Alloc_Count + 1;
      end return;
   end Allocate;

   ----------
   -- Copy --
   ----------

   function Copy
     (This    : in out Instance'Class;
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
     (Core_Size  : Positive;
      Marks      : not null access Skit.Marks.Abstraction'Class)
      return Skit.Allocator.Reference
   is
      Top_Address : constant Cell_Address := Cell_Address (Core_Size - 1);
      Space_Size  : constant Cell_Address := Cell_Address (Core_Size / 2);
      To_Space    : constant Cell_Address := 0;
      From_Space  : constant Cell_Address := Space_Size;

      Mem : constant Skit.Allocator.Reference :=
                      new Instance'
                        (Last        => Top_Address,
                         Marks       => Marks,
                         Core        => <>,
                         Top         => To_Space + Space_Size,
                         Free        => To_Space,
                         From_Space  => From_Space,
                         To_Space    => To_Space,
                         Space_Size  => Space_Size,
                         Scan        => 0,
                         Alloc_Left  => Nil,
                         Alloc_Right => Nil,
                         Alloc_Count => 0,
                         Reclaimed   => 0,
                         GC_Time     => 0.0,
                         GC_Count    => 0);

   begin
      return Mem;
   end Create;

   ----------
   -- Flip --
   ----------

   procedure Flip (This : in out Instance'Class) is
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
     (This : in out Instance'Class)
   is
      use Ada.Calendar;
      Start           : constant Time := Clock;
      Old_Alloc_Count : constant Natural := This.Alloc_Count;
   begin

      This.Flip;

      This.Mark (This.Alloc_Left);
      This.Mark (This.Alloc_Right);

      declare
         procedure Set (Item : in out Object);

         ---------
         -- Set --
         ---------

         procedure Set (Item : in out Object) is
         begin
            This.Mark (Item);
         end Set;

      begin
         This.Marks.Mark (Set'Access);
      end;

      while This.Scan < This.Free loop
         declare
            Cell      : Cell_Type renames This.Core (This.Scan);
            New_Left  : constant Object := This.Move (Cell.Left);
            New_Right : constant Object := This.Move (Cell.Right);
         begin
            Cell := (New_Left, New_Right);
            This.Scan := @ + 1;
         end;
      end loop;

      This.Alloc_Count := Natural (This.Scan - This.To_Space);
      This.Reclaimed := @ + Old_Alloc_Count - This.Alloc_Count;
      This.GC_Time := @ + Clock - Start;
      This.GC_Count := @ + 1;

   end GC;

   -------------------
   -- In_From_Space --
   -------------------

   function In_From_Space
     (This   : Instance'Class;
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
     (This   : Instance'Class;
      Item   : Object)
      return Boolean
   is
   begin
      return Item.Tag = Application_Object
        and then Item.Payload in
          This.To_Space .. This.To_Space + This.Space_Size - 1;
   end In_To_Space;

   ----------
   -- Left --
   ----------

   overriding function Left
     (This : Instance;
      App  : Object)
      return Object
   is
   begin
      if App.Payload > This.Last then
         raise Skit.Exceptions.Address_Error with App.Payload'Image;
      end if;

      return This.Core (App.Payload).Left;
   end Left;

   ----------
   -- Mark --
   ----------

   procedure Mark
     (This : in out Instance'Class;
      Item : in out Object)
   is
   begin
      Item := This.Move (Item);
   end Mark;

   ----------
   -- Move --
   ----------

   function Move
     (This : in out Instance'Class;
      Item : Object)
      return Object
   is
   begin
      if not This.In_From_Space (Item) then
         return Item;
      end if;

      declare
         Address : constant Cell_Address := Item.Payload;
      begin
         if not This.In_To_Space (This.Core (Address).Left) then
            This.Core (Address).Left :=
              (This.Copy (Address), Application_Object);
         end if;
         return This.Core (Address).Left;
      end;

   end Move;

   -----------
   -- Right --
   -----------

   overriding function Right
     (This : Instance;
      App  : Object)
      return Object
   is
   begin
      if App.Payload > This.Last then
         raise Skit.Exceptions.Address_Error with App.Payload'Image;
      end if;

      return This.Core (App.Payload).Right;
   end Right;

   --------------
   -- Set_Left --
   --------------

   overriding procedure Set_Left
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

   overriding procedure Set_Right
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

end Skit.Impl.Memory;
