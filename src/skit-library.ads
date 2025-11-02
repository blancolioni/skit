with Skit.Environment;

package Skit.Library is

   procedure Load_Primitives
     (Environment : not null access Skit.Environment.Instance'Class);

   procedure Load_Standard_Library
     (Environment : not null access Skit.Environment.Instance'Class);

end Skit.Library;
