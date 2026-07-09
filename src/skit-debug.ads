with Skit.Memory;

private package Skit.Debug is

   function Image (X : Object) return String;

   function Image
     (X : Object;
      Core : Skit.Memory.Instance)
      return String;

end Skit.Debug;
