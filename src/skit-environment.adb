with Ada.Strings.Fixed;
with Ada.Text_IO;
with Skit.Compiler;
with Skit.Parser;

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

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This  : in out Instance;
      Name  : String;
      Blob  : not null access Skit.Interfaces.Abstraction'Class)
   is
   begin
      This.Blobs.Insert (Name, Skit.Interfaces.Reference (Blob));
   end Bind;

   ----------
   -- Blob --
   ----------

   function Blob
     (This : Instance;
      Name : String)
      return Skit.Interfaces.Reference
   is
   begin
      return This.Blobs.Element (Name);
   end Blob;

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

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (This : in out Instance;
      Expr : String)
   is
   begin
      This.Parse (Expr);
      This.Machine.Evaluate;
   end Evaluate;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (This : in out Instance;
      Expr : String)
   is
      procedure Bind_Value
        (Name : String;
         Term : Skit.Terms.Term);

      ----------------
      -- Bind_Value --
      ----------------

      procedure Bind_Value
        (Name : String;
         Term : Skit.Terms.Term)
      is
         Compiled : constant Skit.Terms.Term :=
                      Skit.Compiler.Compile (Term);
      begin
         This.Bind
           (Name,
            Skit.Terms.Install
              (Compiled, This'Access, This.Machine));
         This.Machine.Push (Nil);
      end Bind_Value;

      Parsed_Term : constant Skit.Terms.Term :=
                      Skit.Parser.Parse
                        (Expr, Bind_Value'Access);
      Compiled_Term : constant Skit.Terms.Term :=
                        Skit.Compiler.Compile (Parsed_Term);
      Top           : constant Object :=
                        Skit.Terms.Install
                          (Compiled_Term, This'Access, This.Machine);
   begin
      This.Machine.Push (Top);
   end Parse;

   ----------
   -- Load --
   ----------

   procedure Load
     (This : in out Instance;
      Path : String)
   is
      procedure Load_Line (Line : String);

      ---------------
      -- Load_Line --
      ---------------

      procedure Load_Line (Line : String) is
      begin
         This.Evaluate (Line);
         This.Machine.Drop;
      end Load_Line;

      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            S : constant String :=
                  Ada.Strings.Fixed.Trim (Get_Line (File), Ada.Strings.Both);
         begin
            if S /= "" then
               Load_Line (S);
            end if;
         end;
      end loop;
   end Load;

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

   -------------
   -- Resolve --
   -------------

   overriding function Resolve
     (This : Instance;
      Name : String)
      return Object
   is
   begin
      return Value : constant Object := This.Lookup (Name) do
         if Value = Undefined then
            raise Constraint_Error with
              "undefined: " & Name;
         end if;
      end return;
   end Resolve;

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
