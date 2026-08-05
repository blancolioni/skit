with Ada.Strings.Unbounded;
with Interfaces;

package Skit.Handles.Images is

   --  Reader/writer for the external module image format
   --  (skit/docs/module-image-format.md).
   --
   --  MVP scope: a single module's snapshot -- header, string pool, cells,
   --  a named-import relocation table, a symbol table and exports, with
   --  internal-reference relocation.  Cell contents may be applications,
   --  integers, floats, the VM-fixed combinators, symbols (re-interned by name
   --  into the loading handle), primitive functions (emitted as by-name
   --  imports
   --  and resolved against the loading handle's environment), and foreign
   --  objects (serialized as class + bytes + child vector, rebuilt on load by
   --  the class factory registered in the loading handle).  Every image carries
   --  an interface fingerprint (over its export names) and an integrity
   --  checksum, verified on load.  Per-export annotations and the cross-module
   --  (sibling-export) link pass are not yet handled; the writer raises
   --  Image_Error rather than emit an object it cannot round-trip (a primitive
   --  with no bound name, or a cyclic foreign object).

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
   --  room for the image's cells; Image_Error is raised on a malformed image
   --  or a checksum mismatch.

   function Fingerprint (Path : String) return Interfaces.Unsigned_32;
   --  The image's interface fingerprint (over its export names), for
   --  stale-link detection.  Two images with the same export set share a
   --  fingerprint; changing the exports changes it.

   Image_Error : exception;

end Skit.Handles.Images;
