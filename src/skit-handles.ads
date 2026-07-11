private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Fixed.Hash;
private with Skit.Machines;
with Skit.Terms;

package Skit.Handles is

   type Write_Handler is access
     procedure (Ch : Character);

   type Handle is tagged private;

   function New_Handle
     (Core_Size : Natural := 256 * 1024;
      Writer    : Write_Handler := null;
      User_Data : access User_Data_Interface'Class := null)
      return Handle;

   procedure Evaluate
     (This : Handle'Class);
   --  Evaluate the expression on top of the stack

   function Pop
     (This : Handle'Class)
      return Object;
   --  Pop an object from the top of the stack and return it

   function Stack_Empty
     (This : Handle'Class)
      return Boolean;
   --  True when the evaluation stack holds no objects

   procedure Push
     (This : Handle'Class;
      X    : Object);

   procedure Apply (This : Handle'Class);

   function Left
     (This : Handle'Class;
      App  : Object)
      return Object
     with Pre => Is_Application (App);

   function Right
     (This : Handle'Class;
      App  : Object)
      return Object
     with Pre => Is_Application (App);

   function Install
     (This     : Handle'Class;
      Top_Term : Skit.Terms.Term;
      Resolve  : not null access
        function (Name : String) return Object)
      return Object;
   --  Build Top_Term into the machine using raw allocation; no GC can
   --  run, because intermediate objects are held on the Ada call stack
   --  and are invisible to the collector.  Call only on an empty
   --  machine, immediately after a GC, or on a machine whose only
   --  prior operations are other calls to Install.  Running out of
   --  memory here means the machine is too small for the term.

   procedure Install
     (This     : Handle'Class;
      Top_Term : Skit.Terms.Term;
      Resolve  : not null access
        function (Name : String) return Object);
   --  Procedural version of Install, which pushes the result onto the stack

   procedure Write
     (This : Handle'Class;
      Expr : Object);
   --  Write an image of the expression on top of the stack
   --  using Writer.  If Writer was null, nothing is written.

   function Image
     (This : Handle'Class;
      Expr : Object)
      return String;

   function Primitive
     (This      : Handle'Class;
      Primitive : Primitive_Evaluator_Interface'Class)
      return Object;

   procedure Bind
     (This  : Handle'Class;
      Name  : String;
      Value : Object);

   function Lookup
     (This : Handle'Class;
      Name : String)
      return Object;

   procedure Report (This : Handle'Class);

private

   package Symbol_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Object, Ada.Strings.Fixed.Hash, "=");

   package Symbol_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

   type User_Data_Reference is access all User_Data_Interface'Class;

   type Handle_Record (Core_Size : Cell_Address) is limited
      record
         Machine   : Skit.Machines.Instance (Core_Size);
         Writer    : Write_Handler;
         Map       : Symbol_Maps.Map;
         Vector    : Symbol_Vectors.Vector;
         User_Data : User_Data_Reference;
      end record;

   type Handle_Access is access Handle_Record;

   type Handle is tagged
      record
         H : Handle_Access;
      end record;

end Skit.Handles;
