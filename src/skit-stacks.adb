package body Skit.Stacks is

   ----------
   -- Drop --
   ----------

   procedure Drop
     (This : in out Abstraction'Class)
   is
      X : constant Object := This.Pop;
   begin
      pragma Unreferenced (X);
   end Drop;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (This  : in out Abstraction'Class;
      Value : out Object)
   is
      X : Object_Array := [Value];
   begin
      if not This.Pop (X) then
         raise Constraint_Error with "pop: empty stack";
      end if;
      Value := X (1);
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop
     (This : in out Abstraction'Class)
      return Object
   is
      X : Object;
   begin
      This.Pop (X);
      return X;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (This   : in out Abstraction'Class;
      Value : Object)
   is
      X : Object_Array (1 .. 1) := [Value];
   begin
      This.Push (X);
   end Push;

end Skit.Stacks;
