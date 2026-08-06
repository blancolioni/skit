with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Skit.Machines;

package body Skit.Handles.Images is

   use Interfaces;
   use type Ada.Containers.Count_Type;
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
   Kind_Symbol      : constant := 4;
   Kind_Foreign     : constant := 5;

   Section_Pool        : constant := 0;
   Section_Cells       : constant := 1;
   Section_Exports     : constant := 2;
   Section_Import      : constant := 3;
   Section_Symbols     : constant := 4;
   Section_Annotations : constant := 5;
   Section_Foreign     : constant := 6;
   Section_Fingerprint : constant := 7;

   Magic : constant String := "SKIX";

   Hash_FNV1a_32 : constant := 1;   --  fingerprint / checksum algorithm id

   --  FNV-1a (32-bit): a small, dependency-free rolling hash used for both the
   --  interface fingerprint and the integrity checksum.
   FNV_Offset : constant Unsigned_32 := 16#811C_9DC5#;
   FNV_Prime  : constant Unsigned_32 := 16#0100_0193#;

   procedure Hash_Byte (H : in out Unsigned_32; B : Byte) is
   begin
      H := (H xor Unsigned_32 (B)) * FNV_Prime;
   end Hash_Byte;

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
     (D   : Byte_Array;
      C   : in out Offset;
      A   : Object_Vectors.Vector;
      Sym : Object_Vectors.Vector;
      Frn : Object_Vectors.Vector)
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
         when Kind_Symbol =>
            return Sym (Natural (Get_U32 (D, C)));
         when Kind_Foreign =>
            return Frn (Natural (Get_U32 (D, C)));
         when others =>
            raise Image_Error with "bad object kind" & Kind'Image;
      end case;
   end Get_Object;

   -----------
   -- Write --
   -----------

   procedure Write
     (This          : Handle'Class;
      Path          : String;
      Exports       : Name_Array;
      Module_Name   : String := "module";
      Annotation_Of : access function (Export_Name : String)
                        return Ada.Streams.Stream_Element_Array := null)
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

      package Prim_Name_Maps is
        new Ada.Containers.Ordered_Maps (Object_Payload, Unbounded_String);

      --  A named import: the (cell, side) slot holds the Undefined sentinel in
      --  the Cells section and is resolved by Name at load.
      type Import_Entry is
         record
            Cell_Index : Natural;
            Side       : Unsigned_8;
            Name       : Unbounded_String;
         end record;

      package Import_Vectors is
        new Ada.Containers.Vectors (Natural, Import_Entry);

      package Payload_Sets is
        new Ada.Containers.Ordered_Sets (Object_Payload);

      --  Per-export annotation bytes, collected once and shared by the
      --  Annotations section and the interface fingerprint below.  The
      --  vector is empty for an export with no annotation (Annotation_Of is
      --  null, or returned a zero-length result) -- it still contributes an
      --  entry (with empty bytes) so the fingerprint covers every export.
      type Ann_Info is
         record
            Bytes : Byte_Vectors.Vector;
         end record;

      package Name_Ann_Maps is
        new Ada.Containers.Indefinite_Ordered_Maps (String, Ann_Info);

      --  A foreign object to serialize: its bound reference and its Object
      --  children (collected via Visit), in Visit order.
      type Foreign_Rec is
         record
            Ref  : Foreign_Reference;
            Kids : Object_Vectors.Vector;
         end record;

      package Foreign_Vectors is
        new Ada.Containers.Vectors (Natural, Foreign_Rec);

      Pool          : Byte_Vectors.Vector;
      Pool_Names    : Name_Offset_Maps.Map;
      Nodes         : Node_Vectors.Vector;
      Id_Of         : Id_Maps.Map;
      Reverse_Names : Prim_Name_Maps.Map;
      Imports       : Import_Vectors.Vector;
      Sym_Ids       : Id_Maps.Map;              --  symbol payload -> local id
      Sym_List      : Object_Vectors.Vector;    --  local id -> symbol object
      Foreign_Ids   : Id_Maps.Map;              --  foreign payload -> local id
      Foreign_List  : Foreign_Vectors.Vector;   --  local id -> foreign record
      Visiting      : Payload_Sets.Set;         --  foreign cycle guard

      function Intern (Name : String) return Unsigned_32;
      procedure Put_Object (B : in out Byte_Vectors.Vector; O : Object);
      procedure Put_Slot
        (B          : in out Byte_Vectors.Vector;
         O          : Object;
         Cell_Index : Natural;
         Side       : Unsigned_8);
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
         elsif Is_Symbol (O) then
            --  Serialize a symbol by name: assign a local id (emitted in the
            --  Symbols section) and re-intern it into the loading handle.
            Put_U8 (B, Kind_Symbol);
            if not Sym_Ids.Contains (Payload (O)) then
               Sym_Ids.Insert (Payload (O), Natural (Sym_List.Length));
               Sym_List.Append (O);
            end if;
            Put_U32 (B, Unsigned_32 (Sym_Ids.Element (Payload (O))));
         elsif Is_Foreign_Object (O) then
            --  A foreign object is assigned its local id during Scan.
            Put_U8 (B, Kind_Foreign);
            Put_U32 (B, Unsigned_32 (Foreign_Ids.Element (Payload (O))));
         else
            raise Image_Error with
              "cannot serialize object (primitive function): "
              & This.Image (O);
         end if;
      end Put_Object;

      --------------
      -- Put_Slot --
      --------------

      --  A cell slot.  A primitive function is build-specific, so it is not
      --  baked into the cell: the slot gets the Undefined sentinel and a named
      --  import entry, resolved by name at load (see ADR 0002).

      procedure Put_Slot
        (B          : in out Byte_Vectors.Vector;
         O          : Object;
         Cell_Index : Natural;
         Side       : Unsigned_8)
      is
      begin
         if Is_Primitive_Function (O) then
            declare
               Pos : constant Prim_Name_Maps.Cursor :=
                       Reverse_Names.Find (Payload (O));
            begin
               if not Prim_Name_Maps.Has_Element (Pos) then
                  raise Image_Error with
                    "primitive function has no bound name to import as: "
                    & This.Image (O);
               end if;
               Put_U8 (B, Kind_Combinator);
               Put_U32 (B, Unsigned_32 (Payload_Undefined));
               Imports.Append
                 (Import_Entry'(Cell_Index, Side,
                                Prim_Name_Maps.Element (Pos)));
            end;
         else
            Put_Object (B, O);
         end if;
      end Put_Slot;

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
         elsif Is_Foreign_Object (O)
           and then not Foreign_Ids.Contains (Payload (O))
         then
            if Visiting.Contains (Payload (O)) then
               raise Image_Error with
                 "cyclic foreign object not supported: " & This.Image (O);
            end if;
            Visiting.Insert (Payload (O));
            declare
               Ref  : constant Foreign_Reference :=
                        This.H.Machine.Foreign_Object_Ref (O);
               Kids : Object_Vectors.Vector;

               procedure Collect (Child : in out Object);

               procedure Collect (Child : in out Object) is
               begin
                  Kids.Append (Child);
               end Collect;
            begin
               Ref.Visit (Collect'Access);
               for Kid of Kids loop
                  Scan (Kid);   --  nested foreign objects get lower ids
               end loop;
               Visiting.Delete (Payload (O));
               Foreign_Ids.Insert
                 (Payload (O), Natural (Foreign_List.Length));
               Foreign_List.Append (Foreign_Rec'(Ref => Ref, Kids => Kids));
            end;
         end if;
      end Scan;

      Cells   : Byte_Vectors.Vector;
      Imp     : Byte_Vectors.Vector;
      Frn     : Byte_Vectors.Vector;
      Syms    : Byte_Vectors.Vector;
      Exp     : Byte_Vectors.Vector;
      Ann     : Byte_Vectors.Vector;
      Fp      : Byte_Vectors.Vector;
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

      --  Build the reverse map object -> bound name over this handle's known
      --  names, so a baked primitive can be re-exported as a named import.
      for K in 1 .. Natural (This.H.Vector.Length) loop
         declare
            Name  : constant String := This.H.Vector (K - 1);
            Value : constant Object := This.Lookup (Name);
         begin
            if Value /= Undefined and then Is_Primitive_Function (Value)
              and then not Reverse_Names.Contains (Payload (Value))
            then
               Reverse_Names.Insert
                 (Payload (Value), To_Unbounded_String (Name));
            end if;
         end;
      end loop;

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

      --  Cells section (import slots recorded as they are emitted).
      Put_U32 (Cells, Unsigned_32 (Nodes.Length));
      declare
         Index : Natural := 0;
      begin
         for N of Nodes loop
            Put_Slot (Cells, N.Left, Index, 0);
            Put_Slot (Cells, N.Right, Index, 1);
            Index := Index + 1;
         end loop;
      end;

      --  Import relocation section: (cell, side) -> name.
      Put_U32 (Imp, Unsigned_32 (Imports.Length));
      for E of Imports loop
         Put_U32 (Imp, Unsigned_32 (E.Cell_Index));
         Put_U8 (Imp, E.Side);
         Put_U32 (Imp, Intern (To_String (E.Name)));
      end loop;

      --  Foreign-object section: class name, relocated children, opaque bytes.
      Put_U32 (Frn, Unsigned_32 (Foreign_List.Length));
      for R of Foreign_List loop
         Put_U32 (Frn, Intern (R.Ref.Class_Name));
         Put_U32 (Frn, Unsigned_32 (R.Kids.Length));
         for Kid of R.Kids loop
            Put_Object (Frn, Kid);
         end loop;
         declare
            Bytes : constant Byte_Array := R.Ref.Serialize;
         begin
            Put_U64 (Frn, Unsigned_64 (Bytes'Length));
            for X of Bytes loop
               Frn.Append (X);
            end loop;
         end;
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

      --  Symbols section: local id -> name.  Emitted last, once every cell and
      --  export slot has been visited and all symbol ids assigned.
      Put_U32 (Syms, Unsigned_32 (Sym_List.Length));
      for K in 1 .. Natural (Sym_List.Length) loop
         declare
            Id : constant Natural := K - 1;
         begin
            Put_U32 (Syms, Unsigned_32 (Id));
            Put_U32
              (Syms, Intern (This.H.Vector (Symbol_Index (Sym_List (Id)))));
         end;
      end loop;

      --  Collect each export's annotation bytes once, keyed and sorted by
      --  name (Ordered_Maps iterates in key order).  Every export gets an
      --  entry, even with empty Bytes, so the fingerprint below covers the
      --  full export set regardless of which ones carry an annotation.
      declare
         Ann_Map : Name_Ann_Maps.Map;
      begin
         for E of Exports loop
            declare
               Name : constant String := To_String (E);
            begin
               if not Ann_Map.Contains (Name) then
                  declare
                     Info : Ann_Info;
                  begin
                     if Annotation_Of /= null then
                        for X of Annotation_Of (Name) loop
                           Info.Bytes.Append (X);
                        end loop;
                     end if;
                     Ann_Map.Insert (Name, Info);
                  end;
               end if;
            end;
         end loop;

         --  Annotations section: per-export opaque bytes.  An export with no
         --  bytes is simply absent from this section (annotations are
         --  optional per the format).
         declare
            Count : Natural := 0;
         begin
            for C in Ann_Map.Iterate loop
               if Name_Ann_Maps.Element (C).Bytes.Length > 0 then
                  Count := Count + 1;
               end if;
            end loop;
            Put_U32 (Ann, Unsigned_32 (Count));
            for C in Ann_Map.Iterate loop
               declare
                  Info : constant Ann_Info := Name_Ann_Maps.Element (C);
               begin
                  if Info.Bytes.Length > 0 then
                     Put_U32 (Ann, Intern (Name_Ann_Maps.Key (C)));
                     Put_U64 (Ann, Unsigned_64 (Info.Bytes.Length));
                     for X of Info.Bytes loop
                        Ann.Append (X);
                     end loop;
                  end if;
               end;
            end loop;
         end;

         --  Interface fingerprint: FNV-1a over the sorted export names and
         --  their annotation bytes, for stale-link detection.
         declare
            H : Unsigned_32 := FNV_Offset;
         begin
            for C in Ann_Map.Iterate loop
               for Ch of Name_Ann_Maps.Key (C) loop
                  Hash_Byte (H, Byte (Character'Pos (Ch)));
               end loop;
               Hash_Byte (H, 0);
               for X of Name_Ann_Maps.Element (C).Bytes loop
                  Hash_Byte (H, X);
               end loop;
               Hash_Byte (H, 0);
            end loop;
            Put_U8 (Fp, Hash_FNV1a_32);
            Put_U8 (Fp, 4);
            Put_U32 (Fp, H);
         end;
      end;

      --  Header: fixed 20 bytes, then a 4-entry section directory.  Interning
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
      Put_U16 (Header, 8);    --  section_count

      declare
         Dir_Size  : constant Natural := 8 * (2 + 8 + 8);
         Base      : constant Natural := 20 + Dir_Size;
         Off_Pool  : constant Natural := Base;
         Off_Cells : constant Natural := Off_Pool + Natural (Pool.Length);
         Off_Imp   : constant Natural := Off_Cells + Natural (Cells.Length);
         Off_Frn   : constant Natural := Off_Imp + Natural (Imp.Length);
         Off_Syms  : constant Natural := Off_Frn + Natural (Frn.Length);
         Off_Exp   : constant Natural := Off_Syms + Natural (Syms.Length);
         Off_Ann   : constant Natural := Off_Exp + Natural (Exp.Length);
         Off_Fp    : constant Natural := Off_Ann + Natural (Ann.Length);
      begin
         Put_U16 (Header, Section_Pool);
         Put_U64 (Header, Unsigned_64 (Off_Pool));
         Put_U64 (Header, Unsigned_64 (Pool.Length));
         Put_U16 (Header, Section_Cells);
         Put_U64 (Header, Unsigned_64 (Off_Cells));
         Put_U64 (Header, Unsigned_64 (Cells.Length));
         Put_U16 (Header, Section_Import);
         Put_U64 (Header, Unsigned_64 (Off_Imp));
         Put_U64 (Header, Unsigned_64 (Imp.Length));
         Put_U16 (Header, Section_Foreign);
         Put_U64 (Header, Unsigned_64 (Off_Frn));
         Put_U64 (Header, Unsigned_64 (Frn.Length));
         Put_U16 (Header, Section_Symbols);
         Put_U64 (Header, Unsigned_64 (Off_Syms));
         Put_U64 (Header, Unsigned_64 (Syms.Length));
         Put_U16 (Header, Section_Exports);
         Put_U64 (Header, Unsigned_64 (Off_Exp));
         Put_U64 (Header, Unsigned_64 (Exp.Length));
         Put_U16 (Header, Section_Annotations);
         Put_U64 (Header, Unsigned_64 (Off_Ann));
         Put_U64 (Header, Unsigned_64 (Ann.Length));
         Put_U16 (Header, Section_Fingerprint);
         Put_U64 (Header, Unsigned_64 (Off_Fp));
         Put_U64 (Header, Unsigned_64 (Fp.Length));
      end;

      --  Assemble the whole image, then append an integrity checksum trailer
      --  (algo, length, sum) computed over everything before it.
      declare
         Full : Byte_Vectors.Vector;
         Sum  : Unsigned_32 := FNV_Offset;

         procedure Append_All (V : Byte_Vectors.Vector);

         procedure Append_All (V : Byte_Vectors.Vector) is
         begin
            for B of V loop
               Full.Append (B);
            end loop;
         end Append_All;
      begin
         Append_All (Header);
         Append_All (Pool);
         Append_All (Cells);
         Append_All (Imp);
         Append_All (Frn);
         Append_All (Syms);
         Append_All (Exp);
         Append_All (Ann);
         Append_All (Fp);
         for B of Full loop
            Hash_Byte (Sum, B);
         end loop;
         Put_U8 (Full, Hash_FNV1a_32);
         Put_U8 (Full, 4);
         Put_U32 (Full, Sum);

         Ada.Streams.Stream_IO.Create
           (File, Ada.Streams.Stream_IO.Out_File, Path);
         Dump (Full);
         Ada.Streams.Stream_IO.Close (File);
      end;
   end Write;

   type Byte_Array_Access is access Byte_Array;

   procedure Free is
     new Ada.Unchecked_Deallocation (Byte_Array, Byte_Array_Access);

   --  What a module keeps between the two link passes: its bytes, its cells,
   --  and where its import table and string pool live.
   type Module_State is
      record
         Data       : Byte_Array_Access;
         Cells      : Object_Vectors.Vector;
         Off_Import : Offset := -1;
         Off_Pool   : Offset := -1;
      end record;

   -----------------
   -- Load_Module --
   -----------------

   --  Pass 1 for one module: read and validate it, materialize its cells,
   --  foreign objects and symbols, back-patch internal references, and bind
   --  its exports -- but leave its imports as sentinels for pass 2.

   procedure Load_Module
     (This       : Handle'Class;
      Path       : String;
      M          : out Module_State;
      Annotation : access procedure
                     (Export_Name : String;
                      Bytes       : Ada.Streams.Stream_Element_Array)
                     := null)
   is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Path);
      declare
         Length : constant Ada.Streams.Stream_IO.Count :=
                    Ada.Streams.Stream_IO.Size (File);
         Last   : Offset;
      begin
         M.Data := new Byte_Array (0 .. Offset (Length) - 1);
         Ada.Streams.Stream_IO.Read (File, M.Data.all, Last);
         Ada.Streams.Stream_IO.Close (File);
      end;

      declare
         D          : Byte_Array renames M.Data.all;
         Sym        : Object_Vectors.Vector;
         Frn        : Object_Vectors.Vector;
         Off_Pool   : Offset := -1;
         Off_Cells  : Offset := -1;
         Off_Exp    : Offset := -1;
         Off_Import : Offset := -1;
         Off_Sym    : Offset := -1;
         Off_Frn    : Offset := -1;
         Off_Ann    : Offset := -1;
         C          : Offset := 0;
      begin
         if D'Length < 26 then
            raise Image_Error with "image too short";
         end if;

         --  Integrity: recompute the checksum over everything but the 6-byte
         --  trailer and compare with the stored sum.
         declare
            Body_Last : constant Offset := D'Last - 6;
            Cursor    : Offset := Body_Last + 1;
            Algo      : constant Unsigned_8 := Get_U8 (D, Cursor);
            Length_B  : constant Unsigned_8 := Get_U8 (D, Cursor);
            Stored    : constant Unsigned_32 := Get_U32 (D, Cursor);
            Sum       : Unsigned_32 := FNV_Offset;
         begin
            pragma Unreferenced (Algo, Length_B);
            for I in D'First .. Body_Last loop
               Hash_Byte (Sum, D (I));
            end loop;
            if Sum /= Stored then
               raise Image_Error with "checksum mismatch";
            end if;
         end;
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
                     when Section_Pool    => Off_Pool   := Off;
                     when Section_Cells   => Off_Cells  := Off;
                     when Section_Exports => Off_Exp    := Off;
                     when Section_Import  => Off_Import := Off;
                     when Section_Symbols => Off_Sym    := Off;
                     when Section_Foreign => Off_Frn    := Off;
                     when Section_Annotations => Off_Ann := Off;
                     when others          => null;
                  end case;
               end;
            end loop;
         end;

         if Off_Pool < 0 or else Off_Cells < 0 or else Off_Exp < 0 then
            raise Image_Error with "missing required section";
         end if;

         --  Symbols: re-intern each by name into this handle, before decoding
         --  any slot that references one.
         if Off_Sym >= 0 then
            declare
               Cursor : Offset := Off_Sym;
               Count  : constant Natural := Natural (Get_U32 (D, Cursor));
            begin
               for J in 1 .. Count loop
                  declare
                     Local_Id : constant Natural :=
                                  Natural (Get_U32 (D, Cursor));
                     Name_Ref : constant Unsigned_32 := Get_U32 (D, Cursor);
                     Name     : constant String :=
                                  Read_Name (D, Off_Pool + Offset (Name_Ref));
                  begin
                     pragma Assert (Local_Id = Natural (Sym.Length));
                     Sym.Append (This.Intern_Symbol (Name));
                  end;
               end loop;
            end;
         end if;

         --  Cells: reserve N blank cells now.  Back-patch is deferred until
         --  foreign objects exist (a cell may reference one).  Like Install,
         --  no collection runs during this build.
         declare
            Cursor : Offset := Off_Cells;
            N      : constant Natural := Natural (Get_U32 (D, Cursor));
         begin
            for J in 1 .. N loop
               M.Cells.Append (This.H.Machine.Append (Skit.I, Skit.I));
            end loop;
         end;

         --  Foreign objects: create each from its class factory in id order.
         --  Post-order ids mean a child (cell -- already reserved -- or a
         --  lower-id foreign object) is available when its parent is built.
         if Off_Frn >= 0 then
            declare
               Cursor : Offset := Off_Frn;
               Count  : constant Natural := Natural (Get_U32 (D, Cursor));
            begin
               for J in 1 .. Count loop
                  declare
                     Class_Ref   : constant Unsigned_32 := Get_U32 (D, Cursor);
                     Class       : constant String :=
                                     Read_Name
                                       (D, Off_Pool + Offset (Class_Ref));
                     Child_Count : constant Natural :=
                                     Natural (Get_U32 (D, Cursor));
                     Children    : Object_Array (1 .. Child_Count);
                  begin
                     for K in Children'Range loop
                        Children (K) :=
                          Get_Object (D, Cursor, M.Cells, Sym, Frn);
                     end loop;
                     declare
                        Len   : constant Natural :=
                                  Natural (Get_U64 (D, Cursor));
                        Bytes : Byte_Array (1 .. Offset (Len));
                        Ref   : Foreign_Reference;
                     begin
                        for K in Bytes'Range loop
                           Bytes (K) := D (Cursor);
                           Cursor := Cursor + 1;
                        end loop;
                        Ref := This.H.Machine.Deserialize_Foreign
                                 (Class, Bytes, Children);
                        if Ref = null then
                           raise Image_Error with
                             "unregistered foreign class: " & Class;
                        end if;
                        Frn.Append (This.H.Machine.Bind_Object (Ref));
                     end;
                  end;
               end loop;
            end;
         end if;

         --  Cell back-patch (symbols and foreign objects now exist).  Import
         --  slots keep their Undefined sentinel until pass 2.
         declare
            Cursor : Offset := Off_Cells + 4;   --  skip the u32 cell count
         begin
            for J in 1 .. Natural (M.Cells.Length) loop
               declare
                  Left  : constant Object :=
                            Get_Object (D, Cursor, M.Cells, Sym, Frn);
                  Right : constant Object :=
                            Get_Object (D, Cursor, M.Cells, Sym, Frn);
               begin
                  This.H.Machine.Set_Left (M.Cells (J - 1), Left);
                  This.H.Machine.Set_Right (M.Cells (J - 1), Right);
               end;
            end loop;
         end;

         --  Exports: bind each into this handle, so a sibling module loaded in
         --  the same pass can resolve an import against it.
         declare
            Cursor : Offset := Off_Exp;
            Count  : constant Natural := Natural (Get_U32 (D, Cursor));
         begin
            for J in 1 .. Count loop
               declare
                  Name_Ref : constant Unsigned_32 := Get_U32 (D, Cursor);
                  Name     : constant String :=
                               Read_Name
                                 (D, Off_Pool + Offset (Name_Ref));
                  Value    : constant Object :=
                               Get_Object (D, Cursor, M.Cells, Sym, Frn);
               begin
                  This.Bind (Name, Value);
               end;
            end loop;
         end;

         --  Annotations: opaque per-export bytes, handed back to the caller
         --  uninterpreted.  No relocation is involved -- these are leaf bytes,
         --  not Objects -- so this can run any time after the pool is known.
         if Off_Ann >= 0 and then Annotation /= null then
            declare
               Cursor : Offset := Off_Ann;
               Count  : constant Natural := Natural (Get_U32 (D, Cursor));
            begin
               for J in 1 .. Count loop
                  declare
                     Name_Ref : constant Unsigned_32 := Get_U32 (D, Cursor);
                     Name     : constant String :=
                                  Read_Name
                                    (D, Off_Pool + Offset (Name_Ref));
                     Len      : constant Natural :=
                                  Natural (Get_U64 (D, Cursor));
                     Bytes    : Byte_Array (1 .. Offset (Len));
                  begin
                     for K in Bytes'Range loop
                        Bytes (K) := D (Cursor);
                        Cursor := Cursor + 1;
                     end loop;
                     Annotation.all (Name, Bytes);
                  end;
               end loop;
            end;
         end if;

         M.Off_Import := Off_Import;
         M.Off_Pool   := Off_Pool;
      end;
   end Load_Module;

   ---------------------
   -- Resolve_Imports --
   ---------------------

   --  Pass 2 for one module: resolve each import name against this handle
   --  (sibling exports were bound in pass 1, ahead of the environment) and
   --  patch the sentinel slot.

   procedure Resolve_Imports
     (This : Handle'Class;
      M    : Module_State)
   is
      D : Byte_Array renames M.Data.all;
   begin
      if M.Off_Import < 0 then
         return;
      end if;
      declare
         Cursor : Offset := M.Off_Import;
         Count  : constant Natural := Natural (Get_U32 (D, Cursor));
      begin
         for J in 1 .. Count loop
            declare
               Cell_Index : constant Natural := Natural (Get_U32 (D, Cursor));
               Side       : constant Unsigned_8 := Get_U8 (D, Cursor);
               Name_Ref   : constant Unsigned_32 := Get_U32 (D, Cursor);
               Name       : constant String :=
                              Read_Name (D, M.Off_Pool + Offset (Name_Ref));
               Value      : constant Object := This.Lookup (Name);
            begin
               if Value = Undefined then
                  raise Image_Error with "unresolved import: " & Name;
               end if;
               if Side = 0 then
                  This.H.Machine.Set_Left (M.Cells (Cell_Index), Value);
               else
                  This.H.Machine.Set_Right (M.Cells (Cell_Index), Value);
               end if;
            end;
         end loop;
      end;
   end Resolve_Imports;

   ----------
   -- Read --
   ----------

   procedure Read
     (This       : Handle'Class;
      Path       : String;
      Annotation : access procedure
                     (Export_Name : String;
                      Bytes       : Ada.Streams.Stream_Element_Array)
                     := null)
   is
      M : Module_State;
   begin
      Load_Module (This, Path, M, Annotation);
      Resolve_Imports (This, M);
      Free (M.Data);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This       : Handle'Class;
      Paths      : Name_Array;
      Annotation : access procedure
                     (Export_Name : String;
                      Bytes       : Ada.Streams.Stream_Element_Array)
                     := null)
   is
      use Ada.Strings.Unbounded;
      Modules : array (Paths'Range) of Module_State;
   begin
      --  Pass 1: materialize every module and register all their exports.
      for I in Paths'Range loop
         Load_Module (This, To_String (Paths (I)), Modules (I), Annotation);
      end loop;
      --  Pass 2: resolve every module's imports against the merged exports.
      for I in Paths'Range loop
         Resolve_Imports (This, Modules (I));
      end loop;
      for I in Paths'Range loop
         Free (Modules (I).Data);
      end loop;
   end Read;

   -----------------
   -- Fingerprint --
   -----------------

   function Fingerprint (Path : String) return Interfaces.Unsigned_32 is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Path);
      declare
         Length : constant Ada.Streams.Stream_IO.Count :=
                    Ada.Streams.Stream_IO.Size (File);
         D      : Byte_Array (0 .. Offset (Length) - 1);
         Last   : Offset;
         C      : Offset := 18;   --  section_count follows the 18-byte prefix
         Off_Fp : Offset := -1;
      begin
         Ada.Streams.Stream_IO.Read (File, D, Last);
         Ada.Streams.Stream_IO.Close (File);
         declare
            Sections : constant Unsigned_16 := Get_U16 (D, C);
         begin
            for J in 1 .. Natural (Sections) loop
               declare
                  Kind : constant Unsigned_16 := Get_U16 (D, C);
                  Off  : constant Offset := Offset (Get_U64 (D, C));
                  Len  : constant Unsigned_64 := Get_U64 (D, C);
               begin
                  pragma Unreferenced (Len);
                  if Kind = Section_Fingerprint then
                     Off_Fp := Off;
                  end if;
               end;
            end loop;
         end;
         if Off_Fp < 0 then
            raise Image_Error with "image has no fingerprint";
         end if;
         declare
            Cursor : Offset := Off_Fp;
            Algo   : constant Unsigned_8 := Get_U8 (D, Cursor);
            Length_B : constant Unsigned_8 := Get_U8 (D, Cursor);
         begin
            pragma Unreferenced (Algo, Length_B);
            return Get_U32 (D, Cursor);
         end;
      end;
   end Fingerprint;

end Skit.Handles.Images;
