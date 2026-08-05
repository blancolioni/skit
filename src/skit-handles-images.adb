with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Interfaces;

with Skit.Machines;

package body Skit.Handles.Images is

   use Interfaces;
   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   subtype Byte is Ada.Streams.Stream_Element;
   subtype Offset is Ada.Streams.Stream_Element_Offset;
   subtype Byte_Array is Ada.Streams.Stream_Element_Array;

   package Byte_Vectors is
     new Ada.Containers.Vectors (Positive, Byte);

   package Object_Vectors is
     new Ada.Containers.Vectors (Natural, Object);

   --  Object kinds in the on-disk encoding (see module-image-format.md; this
   --  is the MVP subset that round-trips without a symbol/import table).
   Kind_Application : constant := 0;
   Kind_Integer     : constant := 1;
   Kind_Float       : constant := 2;
   Kind_Combinator  : constant := 3;

   Section_Pool    : constant := 0;
   Section_Cells   : constant := 1;
   Section_Exports : constant := 2;

   Magic : constant String := "SKIX";

   function LF_To_U64 is
     new Ada.Unchecked_Conversion (Long_Float, Unsigned_64);
   function U64_To_LF is
     new Ada.Unchecked_Conversion (Unsigned_64, Long_Float);
   function I32_To_U32 is
     new Ada.Unchecked_Conversion (Integer_32, Unsigned_32);
   function U32_To_I32 is
     new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);

   ----------------------------------------------------------------------------
   --  Little-endian writers over a growable byte buffer
   ----------------------------------------------------------------------------

   procedure Put_U8 (B : in out Byte_Vectors.Vector; X : Unsigned_8) is
   begin
      B.Append (Byte (X));
   end Put_U8;

   procedure Put_U16 (B : in out Byte_Vectors.Vector; X : Unsigned_16) is
   begin
      for I in 0 .. 1 loop
         B.Append (Byte (Shift_Right (X, I * 8) and 16#FF#));
      end loop;
   end Put_U16;

   procedure Put_U32 (B : in out Byte_Vectors.Vector; X : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         B.Append (Byte (Shift_Right (X, I * 8) and 16#FF#));
      end loop;
   end Put_U32;

   procedure Put_U64 (B : in out Byte_Vectors.Vector; X : Unsigned_64) is
   begin
      for I in 0 .. 7 loop
         B.Append (Byte (Shift_Right (X, I * 8) and 16#FF#));
      end loop;
   end Put_U64;

   procedure Put_I32 (B : in out Byte_Vectors.Vector; X : Integer) is
   begin
      Put_U32 (B, I32_To_U32 (Integer_32 (X)));
   end Put_I32;

   procedure Put_F64 (B : in out Byte_Vectors.Vector; X : Long_Float) is
   begin
      Put_U64 (B, LF_To_U64 (X));
   end Put_F64;

   ----------------------------------------------------------------------------
   --  Little-endian readers over a byte array with a moving cursor
   ----------------------------------------------------------------------------

   function Get_U8 (D : Byte_Array; C : in out Offset) return Unsigned_8 is
   begin
      return R : constant Unsigned_8 := Unsigned_8 (D (C)) do
         C := C + 1;
      end return;
   end Get_U8;

   function Get_U16 (D : Byte_Array; C : in out Offset) return Unsigned_16 is
      R : Unsigned_16 := 0;
   begin
      for I in 0 .. 1 loop
         R := R or Shift_Left (Unsigned_16 (D (C + Offset (I))), I * 8);
      end loop;
      C := C + 2;
      return R;
   end Get_U16;

   function Get_U32 (D : Byte_Array; C : in out Offset) return Unsigned_32 is
      R : Unsigned_32 := 0;
   begin
      for I in 0 .. 3 loop
         R := R or Shift_Left (Unsigned_32 (D (C + Offset (I))), I * 8);
      end loop;
      C := C + 4;
      return R;
   end Get_U32;

   function Get_U64 (D : Byte_Array; C : in out Offset) return Unsigned_64 is
      R : Unsigned_64 := 0;
   begin
      for I in 0 .. 7 loop
         R := R or Shift_Left (Unsigned_64 (D (C + Offset (I))), I * 8);
      end loop;
      C := C + 8;
      return R;
   end Get_U64;

   function Get_I32 (D : Byte_Array; C : in out Offset) return Integer is
   begin
      return Integer (U32_To_I32 (Get_U32 (D, C)));
   end Get_I32;

   function Get_F64 (D : Byte_Array; C : in out Offset) return Long_Float is
   begin
      return U64_To_LF (Get_U64 (D, C));
   end Get_F64;

   ------------------
   -- Read_Name --
   ------------------

   function Read_Name (D : Byte_Array; Pos : Offset) return String is
      C   : Offset := Pos;
      Len : constant Natural := Natural (Get_U16 (D, C));
      R   : String (1 .. Len);
   begin
      for I in R'Range loop
         R (I) := Character'Val (Natural (D (C)));
         C := C + 1;
      end loop;
      return R;
   end Read_Name;

   -----------------------------
   -- Combinator_From_Payload --
   -----------------------------

   function Combinator_From_Payload (P : Object_Payload) return Object is
   begin
      case P is
         when Payload_Nil        => return Nil;
         when Payload_S          => return S;
         when Payload_K          => return K;
         when Payload_I          => return I;
         when Payload_C          => return C;
         when Payload_B          => return B;
         when Payload_S_Prime    => return S_Prime;
         when Payload_B_Star     => return B_Star;
         when Payload_C_Prime    => return C_Prime;
         when Payload_Y          => return Y;
         when Payload_Undefined  => return Undefined;
         when Payload_Suspension => return Suspension;
         when others =>
            raise Image_Error with
              "unknown combinator payload" & P'Image;
      end case;
   end Combinator_From_Payload;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
     (D : Byte_Array;
      C : in out Offset;
      A : Object_Vectors.Vector)
      return Object
   is
      Kind : constant Unsigned_8 := Get_U8 (D, C);
   begin
      case Kind is
         when Kind_Application =>
            return A (Natural (Get_U32 (D, C)));
         when Kind_Integer =>
            return To_Object (Get_I32 (D, C));
         when Kind_Float =>
            return To_Object (Get_F64 (D, C));
         when Kind_Combinator =>
            return Combinator_From_Payload (Object_Payload (Get_U32 (D, C)));
         when others =>
            raise Image_Error with "bad object kind" & Kind'Image;
      end case;
   end Get_Object;

   -----------
   -- Write --
   -----------

   procedure Write
     (This        : Handle'Class;
      Path        : String;
      Exports     : Name_Array;
      Module_Name : String := "module")
   is
      use Ada.Strings.Unbounded;

      package Id_Maps is
        new Ada.Containers.Ordered_Maps (Object_Payload, Natural);

      package Name_Offset_Maps is
        new Ada.Containers.Indefinite_Ordered_Maps (String, Natural);

      type Node is
         record
            Left, Right : Object;
         end record;

      package Node_Vectors is
        new Ada.Containers.Vectors (Natural, Node);

      Pool       : Byte_Vectors.Vector;
      Pool_Names : Name_Offset_Maps.Map;
      Nodes      : Node_Vectors.Vector;
      Id_Of      : Id_Maps.Map;

      function Intern (Name : String) return Unsigned_32;
      procedure Put_Object (B : in out Byte_Vectors.Vector; O : Object);
      procedure Scan (O : Object);

      ------------
      -- Intern --
      ------------

      function Intern (Name : String) return Unsigned_32 is
         Pos : constant Name_Offset_Maps.Cursor := Pool_Names.Find (Name);
      begin
         if Name_Offset_Maps.Has_Element (Pos) then
            return Unsigned_32 (Name_Offset_Maps.Element (Pos));
         end if;
         return Off : constant Unsigned_32 := Unsigned_32 (Pool.Length) do
            Put_U16 (Pool, Unsigned_16 (Name'Length));
            for Ch of Name loop
               Pool.Append (Byte (Character'Pos (Ch)));
            end loop;
            Pool_Names.Insert (Name, Natural (Off));
         end return;
      end Intern;

      ----------------
      -- Put_Object --
      ----------------

      procedure Put_Object (B : in out Byte_Vectors.Vector; O : Object) is
      begin
         if Is_Application (O) then
            Put_U8 (B, Kind_Application);
            Put_U32 (B, Unsigned_32 (Id_Of.Element (Payload (O))));
         elsif Is_Integer (O) then
            Put_U8 (B, Kind_Integer);
            Put_I32 (B, To_Integer (O));
         elsif Is_Float (O) then
            Put_U8 (B, Kind_Float);
            Put_F64 (B, To_Float (O));
         elsif Is_Primitive (O) and then Payload (O) <= Payload_Suspension then
            Put_U8 (B, Kind_Combinator);
            Put_U32 (B, Unsigned_32 (Payload (O)));
         else
            raise Image_Error with
              "cannot serialize object (symbol, foreign, or primitive"
              & " function): " & This.Image (O);
         end if;
      end Put_Object;

      ----------
      -- Scan --
      ----------

      procedure Scan (O : Object) is
      begin
         if Is_Application (O) and then not Id_Of.Contains (Payload (O)) then
            declare
               Idx : constant Natural := Natural (Nodes.Length);
               L   : constant Object := This.H.Machine.Left (O);
               R   : constant Object := This.H.Machine.Right (O);
            begin
               Id_Of.Insert (Payload (O), Idx);
               Nodes.Append (Node'(Left => L, Right => R));
               Scan (L);
               Scan (R);
            end;
         end if;
      end Scan;

      Cells   : Byte_Vectors.Vector;
      Exp     : Byte_Vectors.Vector;
      Header  : Byte_Vectors.Vector;
      Mod_Ref : Unsigned_32;

      File : Ada.Streams.Stream_IO.File_Type;

      procedure Dump (V : Byte_Vectors.Vector);

      ----------
      -- Dump --
      ----------

      procedure Dump (V : Byte_Vectors.Vector) is
         Buf : Byte_Array (1 .. Offset (V.Length));
      begin
         for I in Buf'Range loop
            Buf (I) := V (Positive (I));
         end loop;
         Ada.Streams.Stream_IO.Write (File, Buf);
      end Dump;

   begin
      Mod_Ref := Intern (Module_Name);

      for E of Exports loop
         declare
            Name : constant String := To_String (E);
            Root : constant Object := This.Lookup (Name);
         begin
            if Root = Undefined then
               raise Image_Error with "unknown export: " & Name;
            end if;
            Scan (Root);
         end;
      end loop;

      --  Cells section.
      Put_U32 (Cells, Unsigned_32 (Nodes.Length));
      for N of Nodes loop
         Put_Object (Cells, N.Left);
         Put_Object (Cells, N.Right);
      end loop;

      --  Exports section: name reference + the (localized) root object.
      Put_U32 (Exp, Unsigned_32 (Exports'Length));
      for E of Exports loop
         declare
            Name : constant String := To_String (E);
         begin
            Put_U32 (Exp, Intern (Name));
            Put_Object (Exp, This.Lookup (Name));
         end;
      end loop;

      --  Header: fixed 20 bytes, then a 3-entry section directory.  Interning
      --  above is complete, so the pool is final and its offsets are stable.
      for Ch of Magic loop
         Put_U8 (Header, Unsigned_8 (Character'Pos (Ch)));
      end loop;
      Put_U16 (Header, 1);    --  format_version
      Put_U16 (Header, 1);    --  vm_version
      Put_U8 (Header, 32);    --  word_size
      Put_U8 (Header, 2);     --  tag_size
      Put_U8 (Header, 0);     --  tag_layout
      Put_U8 (Header, 0);     --  endianness (little)
      Put_U16 (Header, 0);    --  flags
      Put_U32 (Header, Mod_Ref);
      Put_U16 (Header, 3);    --  section_count

      declare
         Dir_Size  : constant Natural := 3 * (2 + 8 + 8);
         Base      : constant Natural := 20 + Dir_Size;
         Off_Pool  : constant Natural := Base;
         Off_Cells : constant Natural := Off_Pool + Natural (Pool.Length);
         Off_Exp   : constant Natural := Off_Cells + Natural (Cells.Length);
      begin
         Put_U16 (Header, Section_Pool);
         Put_U64 (Header, Unsigned_64 (Off_Pool));
         Put_U64 (Header, Unsigned_64 (Pool.Length));
         Put_U16 (Header, Section_Cells);
         Put_U64 (Header, Unsigned_64 (Off_Cells));
         Put_U64 (Header, Unsigned_64 (Cells.Length));
         Put_U16 (Header, Section_Exports);
         Put_U64 (Header, Unsigned_64 (Off_Exp));
         Put_U64 (Header, Unsigned_64 (Exp.Length));
      end;

      Ada.Streams.Stream_IO.Create
        (File, Ada.Streams.Stream_IO.Out_File, Path);
      Dump (Header);
      Dump (Pool);
      Dump (Cells);
      Dump (Exp);
      Ada.Streams.Stream_IO.Close (File);
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
     (This : Handle'Class;
      Path : String)
   is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Path);

      declare
         Length : constant Ada.Streams.Stream_IO.Count :=
                    Ada.Streams.Stream_IO.Size (File);
         D      : Byte_Array (0 .. Offset (Length) - 1);
         Last   : Offset;

         A         : Object_Vectors.Vector;
         Off_Pool  : Offset := -1;
         Off_Cells : Offset := -1;
         Off_Exp   : Offset := -1;
         C         : Offset := 0;
      begin
         Ada.Streams.Stream_IO.Read (File, D, Last);
         Ada.Streams.Stream_IO.Close (File);

         if D'Length < 20 then
            raise Image_Error with "image too short";
         end if;
         for J in Magic'Range loop
            if D (Offset (J - Magic'First))
              /= Byte (Character'Pos (Magic (J)))
            then
               raise Image_Error with "bad magic";
            end if;
         end loop;

         C := 4;
         declare
            Format    : constant Unsigned_16 := Get_U16 (D, C);
            Unused_Vm : constant Unsigned_16 := Get_U16 (D, C);
            Word_Size : constant Unsigned_8  := Get_U8 (D, C);
            Unused_Ts : constant Unsigned_8  := Get_U8 (D, C);
            Tag_Layout : constant Unsigned_8 := Get_U8 (D, C);
            Endianness : constant Unsigned_8 := Get_U8 (D, C);
            Unused_Fl : constant Unsigned_16 := Get_U16 (D, C);
            Unused_Mn : constant Unsigned_32 := Get_U32 (D, C);
            Sections  : constant Unsigned_16 := Get_U16 (D, C);
         begin
            pragma Unreferenced (Unused_Vm, Unused_Ts, Unused_Fl, Unused_Mn);
            if Format /= 1 then
               raise Image_Error with "unsupported format version";
            end if;
            if Word_Size /= 32 or else Tag_Layout /= 0
              or else Endianness /= 0
            then
               raise Image_Error with "unsupported object representation";
            end if;

            for J in 1 .. Natural (Sections) loop
               declare
                  Kind : constant Unsigned_16 := Get_U16 (D, C);
                  Off  : constant Offset := Offset (Get_U64 (D, C));
                  Len  : constant Unsigned_64 := Get_U64 (D, C);
               begin
                  pragma Unreferenced (Len);
                  case Kind is
                     when Section_Pool    => Off_Pool  := Off;
                     when Section_Cells   => Off_Cells := Off;
                     when Section_Exports => Off_Exp   := Off;
                     when others          => null;
                  end case;
               end;
            end loop;
         end;

         if Off_Pool < 0 or else Off_Cells < 0 or else Off_Exp < 0 then
            raise Image_Error with "missing required section";
         end if;

         --  Cells: reserve N blank cells, then back-patch each from the image.
         --  Like Install, no collection runs during this build.
         declare
            Cursor : Offset := Off_Cells;
            N      : constant Natural := Natural (Get_U32 (D, Cursor));
         begin
            for J in 1 .. N loop
               A.Append (This.H.Machine.Append (Skit.I, Skit.I));
            end loop;
            for J in 1 .. N loop
               declare
                  Left  : constant Object := Get_Object (D, Cursor, A);
                  Right : constant Object := Get_Object (D, Cursor, A);
               begin
                  This.H.Machine.Set_Left (A (J - 1), Left);
                  This.H.Machine.Set_Right (A (J - 1), Right);
               end;
            end loop;
         end;

         --  Exports: bind each into this handle.
         declare
            Cursor : Offset := Off_Exp;
            M      : constant Natural := Natural (Get_U32 (D, Cursor));
         begin
            for J in 1 .. M loop
               declare
                  Name_Ref : constant Unsigned_32 := Get_U32 (D, Cursor);
                  Name     : constant String :=
                               Read_Name
                                 (D, Off_Pool + Offset (Name_Ref));
                  Value    : constant Object := Get_Object (D, Cursor, A);
               begin
                  This.Bind (Name, Value);
               end;
            end loop;
         end;
      end;
   end Read;

end Skit.Handles.Images;
