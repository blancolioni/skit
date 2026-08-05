with Ada.Strings.Unbounded;

package Skit.Handles.Images is

   --  Reader/writer for the external module image format
   --  (skit/docs/module-image-format.md).
   --
   --  MVP scope: a single module's snapshot -- header, string pool, cells,
   --  a named-import relocation table and exports, with internal-reference
   --  relocation.  Cell contents may be applications, integers, floats, the
   --  VM-fixed combinators, and primitive functions (emitted as by-name
   --  imports and resolved against the loading handle's environment).  Symbols,
   --  foreign objects, annotations, the interface fingerprint and the checksum
   --  are not yet handled; the writer raises Image_Error rather than emit an
   --  object it cannot round-trip (and on a primitive with no bound name).

   type Name_Array is
     array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Write
     (This        : Handle'Class;
      Path        : String;
      Exports     : Name_Array;
      Module_Name : String := "module");
   --  Write, to Path, a snapshot of the graph reachable from the named exports
   --  (looked up in This).  Raises Image_Error on an unknown export or an
   --  object the MVP format cannot represent.

   procedure Read
     (This : Handle'Class;
      Path : String);
   --  Load the image at Path, binding each of its exports into This.  Like
   --  Install, this allocates without collecting, so call it on a machine with
   --  room for the image's cells; Image_Error is raised on a malformed image.

   Image_Error : exception;

end Skit.Handles.Images;
