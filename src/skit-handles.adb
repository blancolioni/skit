with Skit.Machines.Report;

------------------
-- Skit.Handles --
------------------

package body Skit.Handles is

   function To_Symbol_Object
     (This : Handle'Class;
      Name : String)
      return Object;

   -----------
   -- Apply --
   -----------

   procedure Apply (This : Handle'Class) is
   begin
      This.H.Machine.Apply;
   end Apply;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This  : Handle'Class;
      Name  : String;
      Value : Object)
   is
   begin
      This.H.Machine.Bind (This.To_Symbol_Object (Name), Value);
   end Bind;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (This : Handle'Class)
   is
   begin
      This.H.Machine.Evaluate (This.H.User_Data);
   end Evaluate;

   -----------
   -- Image --
   -----------

   function Image
     (This : Handle'Class;
      Expr : Object)
      return String
   is
   begin
      return This.H.Machine.Debug_Image (Expr);
   end Image;

   -------------
   -- Install --
   -------------

   procedure Install
     (This     : Handle'Class;
      Top_Term : Skit.Terms.Term;
      Resolve  : not null access
        function (Name : String) return Object)
   is
   begin
      This.H.Machine.Push (This.Install (Top_Term, Resolve));
   end Install;

   -------------
   -- Install --
   -------------

   function Install
     (This     : Handle'Class;
      Top_Term : Skit.Terms.Term;
      Resolve  : not null access
        function (Name : String) return Object)
      return Object
   is

      use Skit.Terms;

      function Install (T : Term) return Object;

      -------------
      -- Install --
      -------------

      function Install (T : Term) return Object is
      begin
         if Is_Application (T) then
            return This.H.Machine.Append
              (Install (Get_Left (T)),
               Install (Get_Right (T)));
         elsif Is_Symbol (T) then
            return Resolve (Get_Symbol (T));
         elsif Is_Atom (T) then
            return Get_Atom (T);
         else
            raise Constraint_Error with
              "cannot install term: " & Image (T);
         end if;
      end Install;

   begin
      return Install (Top_Term);
   end Install;

   ----------
   -- Left --
   ----------

   function Left
     (This : Handle'Class;
      App  : Object)
      return Object
   is
   begin
      return This.H.Machine.Left (App);
   end Left;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (This : Handle'Class;
      Name : String)
      return Object
   is
   begin
      return This.H.Machine.Lookup
        (This.To_Symbol_Object (Name));
   end Lookup;

   ----------------
   -- New_Handle --
   ----------------

   function New_Handle
     (Core_Size : Natural := 256 * 1024;
      Writer    : Write_Handler := null;
      User_Data : access User_Data_Interface'Class := null)
      return Handle
   is
      H : constant Handle_Access :=
            new Handle_Record (Cell_Address (Core_Size - 1));
   begin
      H.Writer := Writer;
      H.Machine.Initialize;
      H.User_Data := User_Data_Reference (User_Data);
      return (H => H);
   end New_Handle;

   ---------
   -- Pop --
   ---------

   function Pop
     (This : Handle'Class)
      return Object
   is
   begin
      return This.H.Machine.Pop;
   end Pop;

   ---------------
   -- Primitive --
   ---------------

   function Primitive
     (This      : Handle'Class;
      Primitive : Primitive_Evaluator_Interface'Class)
      return Object
   is
   begin
      return This.H.Machine.Primitive (Primitive);
   end Primitive;

   ----------
   -- Push --
   ----------

   procedure Push
     (This : Handle'Class;
      X    : Object)
   is
   begin
      This.H.Machine.Push (X);
   end Push;

   ------------
   -- Report --
   ------------

   procedure Report (This : Handle'Class) is
   begin
      Skit.Machines.Report (This.H.Machine);
   end Report;

   -----------
   -- Right --
   -----------

   function Right
     (This : Handle'Class;
      App  : Object)
      return Object
   is
   begin
      return This.H.Machine.Left (App);
   end Right;

   ----------------------
   -- To_Symbol_Object --
   ----------------------

   function To_Symbol_Object
     (This : Handle'Class;
      Name : String)
      return Object
   is
      use Symbol_Maps;
      Position : constant Cursor := This.H.Map.Find (Name);
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         This.H.Vector.Append (Name);
         return Symbol : constant Object :=
           (Object_Payload (This.H.Vector.Last_Index)
            + Primitive_Variable_Payload'First,
            Primitive_Object)
         do
            This.H.Map.Insert (Name, Symbol);
         end return;
      end if;
   end To_Symbol_Object;

   -----------
   -- Write --
   -----------

   procedure Write
     (This : Handle'Class;
      Expr : Object)
   is
      S : constant String := This.Image (Expr);
   begin
      if This.H.Writer /= null then
         for Ch of S loop
            This.H.Writer (Ch);
         end loop;
      end if;
   end Write;

end Skit.Handles;
