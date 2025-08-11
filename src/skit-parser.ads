with Skit.Machine;

package Skit.Parser is

   Parse_Error : exception;

   procedure Parse
     (Source    : String;
      To_Object : not null access
        function (X : String) return Object;
      Bind_Value : not null access
        procedure (Name : String);
      Machine   : not null access Skit.Machine.Abstraction'Class);

end Skit.Parser;
