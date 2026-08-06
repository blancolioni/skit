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
   --  the class factory registered in the loading handle).  Each image carries
   --  an interface fingerprint (over its export names) and an integrity
   --  checksum, verified on load.  Several modules can be loaded and linked
   --  together (two-pass, sibling exports resolving before the environment; see
   --  the Name_Array Read).  Per-export annotations are not yet handled; the
   --  writer raises Image_Error rather than emit an object it cannot round-trip
   --  (a primitive with no bound name, or a cyclic foreign object).

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

   procedure Read
     (This  : Handle'Class;
      Paths : Name_Array);
   --  Load several images together and link them: a two-pass load registers
   --  every module's exports first, then resolves every module's imports.
   --  An import resolves to a sibling module's export in preference to the
   --  standing environment, so the modules may reference one another mutually.
   --  Room for every module's cells must exist up front (no collection runs).

   function Fingerprint (Path : String) return Interfaces.Unsigned_32;
   --  The image's interface fingerprint (over its export names), for
   --  stale-link detection.  Two images with the same export set share a
   --  fingerprint; changing the exports changes it.

   Image_Error : exception;

end Skit.Handles.Images;
