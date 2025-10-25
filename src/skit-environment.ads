private with Ada.Containers.Indefinite_Ordered_Maps;

with Skit.Containers;
with Skit.Machine;
with Skit.Primitives;

package Skit.Environment is

   type Instance is new Skit.Containers.Abstraction with private;
   type Reference is access all Instance'Class;

   function Create
     (Machine : Skit.Machine.Reference)
      return Reference;

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

   function Lookup
     (This : Instance;
      Name : String)
      return Object;

   function To_Symbol_Object
     (This : in out Instance;
      Name : String)
      return Object;

private

   package Binding_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (String, Object, "<");

   type Instance is new Skit.Containers.Abstraction with
      record
         Machine  : Skit.Machine.Reference;
         Bindings : Binding_Maps.Map;
         Next_Symbol : Object_Payload :=
                         Primitive_Variable_Payload'First;
      end record;

   overriding procedure Mark
     (This : in out Instance;
      Set  : not null access
        procedure (Item : in out Object));

end Skit.Environment;
