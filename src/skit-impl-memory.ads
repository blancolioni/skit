private with Ada.Containers.Doubly_Linked_Lists;
private with Skit.Containers;

private package Skit.Impl.Memory is

   type Instance (<>) is private;
   type Reference is access all Instance;

   function Create
     (Core_Size  : Positive)
      return Reference;

   procedure Add_Container
     (This      : in out Instance;
      Container : not null access Skit.Containers.Abstraction'Class);

   function Allocate
     (This   : in out Instance;
      Left   : in out Object;
      Right  : in out Object)
      return Object;

   function Left
     (This : Instance;
      App  : Object)
      return Object
     with Inline;

   function Right
     (This : Instance;
      App  : Object)
      return Object
     with Inline;

   procedure Set_Left
     (This : in out Instance;
      App  : Object;
      To   : Object);

   procedure Set_Right
     (This : in out Instance;
      App  : Object;
      To   : Object);

   procedure Report (This : Instance);

   procedure Trace_GC
     (This    : in out Instance;
      Enabled : Boolean);

private

   subtype Cell_Address is Object_Payload;

   type Cell_Type is
      record
         Left, Right : Object;
      end record;

   type Cell_Array is array (Cell_Address range <>) of Cell_Type;

   type Container_Reference is access all Skit.Containers.Abstraction'Class;
   package Container_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Container_Reference);

   type Instance (Last : Cell_Address) is
      record
         Core              : Cell_Array (0 .. Last);
         Top               : Cell_Address;
         Free              : Cell_Address;
         From_Space        : Cell_Address;
         To_Space          : Cell_Address;
         Space_Size        : Cell_Address;
         Scan              : Cell_Address;
         Containers        : Container_Lists.List;
         Alloc_Left        : Object;
         Alloc_Right       : Object;
         Trace_GC          : Boolean := False;
         Alloc_Count       : Natural := 0;
         Active_Cells      : Natural := 0;
         Max_Active_Cells  : Natural := 0;
         Total_Alloc_Count : Natural := 0;
         Reclaimed         : Natural := 0;
         GC_Time           : Duration := 0.0;
         GC_Count          : Natural := 0;
      end record;

   function Left
     (This : Instance;
      App  : Object)
      return Object
   is (This.Core (App.Payload).Left);

   function Right
     (This : Instance;
      App  : Object)
      return Object
   is (This.Core (App.Payload).Right);

   function Quick_Allocate
     (This   : in out Instance;
      Left   : Object;
      Right  : Object)
      return Object
     with Inline_Always;

   function Free_And_Allocate
     (This   : in out Instance;
      Left   : in out Object;
      Right  : in out Object)
      return Object;

   function Allocate
     (This   : in out Instance;
      Left   : in out Object;
      Right  : in out Object)
      return Object
   is (if This.Free < This.Top
       then Quick_Allocate (This, Left, Right)
       else Free_And_Allocate (This, Left, Right));

end Skit.Impl.Memory;
