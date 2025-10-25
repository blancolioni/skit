package body Skit.Environment is

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This      : in out Instance;
      Name      : String;
      Primitive : Skit.Primitives.Abstraction'Class)
   is
      P : constant Object := This.Machine.Bind (Primitive);
   begin
      if This.Bindings.Contains (Name) then
         raise Constraint_Error with
           "redefinition of primitive " & Name;
      end if;
      This.Bindings.Insert (Name, P);
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This  : in out Instance;
      Name  : String;
      Value : Object)
   is
   begin
      if This.Bindings.Contains (Name) then
         if Is_Primitive (This.Bindings (Name)) then
            raise Constraint_Error with
              "redefinition of primitive " & Name;
         end if;
         This.Bindings.Replace (Name, Value);
      else
         This.Bindings.Insert (Name, Value);
      end if;
   end Bind;

   ------------
   -- Create --
   ------------

   function Create
     (Machine : Skit.Machine.Reference)
      return Reference
   is
   begin
      return This : constant Reference := new Instance'
        (Machine  => Machine, others => <>)
      do
         This.Machine.Add_Container (This);
      end return;
   end Create;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (This : Instance;
      Name : String)
      return Object
   is
   begin
      if This.Bindings.Contains (Name) then
         return This.Bindings (Name);
      else
         return Undefined;
      end if;
   end Lookup;

   -------------
   -- Machine --
   -------------

   function Machine
     (This : Instance)
      return Skit.Machine.Reference
   is
   begin
      return This.Machine;
   end Machine;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (This : in out Instance;
      Set  : not null access
        procedure (Item : in out Object))
   is
   begin
      for Binding of This.Bindings loop
         Set (Binding);
      end loop;
   end Mark;

   ----------------------
   -- To_Symbol_Object --
   ----------------------

   function To_Symbol_Object
     (This : in out Instance;
      Name : String)
      return Object
   is
   begin
      if This.Bindings.Contains (Name) then
         return This.Bindings (Name);
      else
         return Sym : constant Object :=
           (This.Next_Symbol, Primitive_Object)
         do
            This.Bindings.Insert (Name, Sym);
            This.Next_Symbol := This.Next_Symbol + 1;
         end return;
      end if;
   end To_Symbol_Object;

end Skit.Environment;
