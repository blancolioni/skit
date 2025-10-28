private with Ada.Containers.Indefinite_Ordered_Maps;

with Skit.Containers;
with Skit.Interfaces;
with Skit.Machine;
with Skit.Primitives;

package Skit.Environment is

   type Instance is new Skit.Containers.Abstraction with private;
   type Reference is access all Instance'Class;

   function Create
     (Machine : Skit.Machine.Reference)
      return Reference;

   procedure Load
     (This : in out Instance;
      Path : String);
   --  load definitions and evaluate expressions from Path

   procedure Evaluate
     (This : in out Instance;
      Expr : String);
   --  Evaluate Expr and leave the result on top of the stack

   function Machine
     (This : Instance)
      return Skit.Machine.Reference;

   procedure Bind
     (This      : in out Instance;
      Name      : String;
      Primitive : Skit.Primitives.Abstraction'Class);

   procedure Bind
     (This  : in out Instance;
      Name  : String;
      Value : Object);

   procedure Bind
     (This  : in out Instance;
      Name  : String;
      Blob  : not null access Skit.Interfaces.Abstraction'Class);

   function Lookup
     (This : Instance;
      Name : String)
      return Object;

   function Blob
     (This : Instance;
      Name : String)
      return Skit.Interfaces.Reference;

   function To_Symbol_Object
     (This : in out Instance;
      Name : String)
      return Object;

private

   package Binding_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (String, Object, "<");

   package Blob_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (String, Skit.Interfaces.Reference, "<", Skit.Interfaces."=");

   type Instance is new Skit.Containers.Abstraction with
      record
         Machine  : Skit.Machine.Reference;
         Bindings : Binding_Maps.Map;
         Blobs    : Blob_Maps.Map;
         Next_Symbol : Object_Payload :=
                         Primitive_Variable_Payload'First;
      end record;

   overriding procedure Mark
     (This : in out Instance;
      Set  : not null access
        procedure (Item : in out Object));

end Skit.Environment;
