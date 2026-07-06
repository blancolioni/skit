with Ada.Unchecked_Deallocation;
with Skit.Debug;

package body Skit.Terms is

   Block_Size : constant := 2 ** 24;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (System.Storage_Elements.Storage_Array,
        Storage_Array_Access);

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool                     : in out Term_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;
   begin
      if Pool.Next mod Alignment /= 0 then
         Pool.Next := Pool.Next + Alignment - Pool.Next mod Alignment;
      end if;

      if Pool.Next + Size_In_Storage_Elements > Pool.Top then
         Pool.Next := Pool.Top;
         Pool.Start := Pool.Top;
         Pool.Block_Index := @ + 1;
         Pool.Memory (Pool.Block_Index) :=
           new Storage_Array (0 .. Block_Size - 1);
         Pool.Top := @ + Block_Size;
      end if;

      Storage_Address :=
        Pool.Memory (Pool.Block_Index) (Pool.Next - Pool.Start)'Address;
      Pool.Next := @ + Size_In_Storage_Elements;
   end Allocate;

   -----------
   -- Image --
   -----------

   function Image (T : Term) return String is
   begin
      case T.Class is
         when Apply =>
            if T.Right.Class = Apply then
               return Image (T.Left) & " (" & Image (T.Right) & ")";
            else
               return Image (T.Left) & " " & Image (T.Right);
            end if;
         when Lambda =>
            return "\" & Get_Variable (T) & "." & Image (T.Expr);
         when Const_Integer =>
            return Integer'Image (T.Integer_Value);
         when Const_Float =>
            return Long_Float'Image (T.Float_Value);
         when Primitive =>
            return Skit.Debug.Image (T.Primitive_Value);
         when Symbol =>
            return Get_Symbol (T);
      end case;
   end Image;

   -------------
   -- Install --
   -------------

   function Install
     (Top_Term : Term;
      Resolver : not null access constant Resolver_Interface'Class;
      Builder  : not null access Skit.Builder.Abstraction'Class)
      return Object
   is
      function Install (T : Term) return Object;

      -------------
      -- Install --
      -------------

      function Install (T : Term) return Object is
      begin
         case T.Class is
            when Apply =>
               return Builder.Build_Apply
                 (Install (T.Left),
                  Install (T.Right));
            when Lambda =>
               raise Constraint_Error with
                 "unexpected lambda while building term";
            when Const_Integer =>
               return To_Object (T.Integer_Value);
            when Const_Float =>
               return To_Object (T.Float_Value);
            when Primitive =>
               return T.Primitive_Value;
            when Symbol =>
               return Resolver.Resolve (Get_Symbol (T));
         end case;
      end Install;

   begin
      return Install (Top_Term);
   end Install;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      for Item of Arena.Memory (1 .. Arena.Block_Index) loop
         Free (Item);
      end loop;

      Arena.Next := 0;
      Arena.Top  := 0;
      Arena.Start := 0;
      Arena.Block_Index := 0;
   end Reset;

   overriding procedure Deallocate
     (Pool                     : in out Term_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is null;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool : Term_Pool)
      return System.Storage_Elements.Storage_Count
   is
      use System.Storage_Elements;
   begin
      return Block_Size * 100;
   end Storage_Size;

end Skit.Terms;
