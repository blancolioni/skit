with Skit.Memory;

package Skit.Debug is

   function Image (X : Object) return String;

   function Image
     (X : Object;
      Core : not null access constant Skit.Memory.Abstraction'Class)
      return String;

end Skit.Debug;
