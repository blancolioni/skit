with Skit.Terms;

package Skit.Parser is

   Parse_Error : exception;

   function Parse
     (Source    : String;
      Bind_Value : access
        procedure (Name : String; Term : Skit.Terms.Term))
      return Skit.Terms.Term;

end Skit.Parser;
