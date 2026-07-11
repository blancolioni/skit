with Skit.Compiler;

package body Skit.Parser is

   -----------
   -- Parse --
   -----------

   function Parse
     (Source    : String;
      Bind_Value : access
        procedure (Name : String; Term : Skit.Terms.Term))
      return Skit.Terms.Term
   is
      Index : Natural := Source'First - 1;
      Ch    : Character := ' ';

      function At_End return Boolean
      is (Index > Source'Last
          or else (Index = Source'Last
              and then Source (Index) = Character'Val (13)));

      function At_Expression return Boolean
      is (not At_End and then Ch /= ')');

      function At_Id return Boolean
      is (not At_End
          and then Ch /= ' '
          and then Ch /= '('
          and then Ch /= ')'
          and then Ch /= '\'
          and then Ch /= '.');

      procedure Skip;

      procedure Skip_Spaces;

      function Parse_Expression return Skit.Terms.Term;
      function Parse_Atomic return Skit.Terms.Term;
      function Parse_Id return String;

      ------------------
      -- Parse_Atomic --
      ------------------

      function Parse_Atomic return Skit.Terms.Term is
      begin
         if Ch = '(' then
            Skip;
            return E : constant Skit.Terms.Term := Parse_Expression do
               if Ch = ')' then
                  Skip;
               else
                  raise Parse_Error with
                    "Position" & Index'Image & ": expected ')'";
               end if;
            end return;
         elsif Ch = '\' then
            Skip;
            declare
               X : constant String := Parse_Id;
            begin
               Skip_Spaces;
               if Ch = '.' then
                  Skip;
               end if;
               declare
                  E : constant Skit.Terms.Term := Parse_Expression;
               begin
                  return Skit.Terms.Lambda (X, E);
               end;
            end;
         else
            declare
               Id : constant String := Parse_Id;
            begin
               if (for all Ch of Id => Ch in '0' .. '9') then
                  return Skit.Terms.Const (Natural'Value (Id));
               elsif Id = "I" then
                  return Skit.Terms.Combinator (Skit.I);
               elsif Id = "S" then
                  return Skit.Terms.Combinator (Skit.S);
               elsif Id = "K" then
                  return Skit.Terms.Combinator (Skit.K);
               elsif Id = "B" then
                  return Skit.Terms.Combinator (Skit.B);
               elsif Id = "C" then
                  return Skit.Terms.Combinator (Skit.C);
               elsif Id = "B*" then
                  return Skit.Terms.Combinator (Skit.B_Star);
               elsif Id = "C'" then
                  return Skit.Terms.Combinator (Skit.C_Prime);
               else
                  return Skit.Terms.Symbol (Id);
               end if;
            end;
         end if;
      end Parse_Atomic;

      ----------------------
      -- Parse_Expression --
      ----------------------

      function Parse_Expression return Skit.Terms.Term is
      begin
         Skip_Spaces;
         if At_End then
                  raise Parse_Error with
                    "Position" & Index'Image & ": expected expression";
         end if;

         declare
            E : Skit.Terms.Term := Parse_Atomic;
         begin
            Skip_Spaces;
            while At_Expression loop
               E := Skit.Terms.Apply (E, Parse_Atomic);
               Skip_Spaces;
            end loop;
            return E;
         end;
      end Parse_Expression;

      --------------
      -- Parse_Id --
      --------------

      function Parse_Id return String is
         Id   : String (1 .. 200);
         Last : Natural := 0;
      begin
         while At_Id loop
            Last := Last + 1;
            Id (Last) := Ch;
            Skip;
         end loop;
         return Id (1 .. Last);
      end Parse_Id;

      ----------
      -- Skip --
      ----------

      procedure Skip is
      begin
         Index := Index + 1;
         if Index <= Source'Last then
            Ch := Source (Index);
         else
            Ch := ' ';
         end if;
      end Skip;

      -----------------
      -- Skip_Spaces --
      -----------------

      procedure Skip_Spaces is
      begin
         while not At_End and then Ch = ' ' loop
            Skip;
         end loop;
      end Skip_Spaces;

   begin
      Skip_Spaces;
      if Ch = '!' then
         Skip;
         declare
            Name : constant String := Parse_Id;
            E    : constant Skit.Terms.Term := Parse_Expression;
            C    : constant Skit.Terms.Term := Skit.Compiler.Compile (E);
         begin
            if Bind_Value /= null then
               Bind_Value (Name, C);
            end if;
            return Skit.Terms.Primitive (Skit.Nil);
         end;
      else
         return Parse_Expression;
      end if;
   end Parse;

end Skit.Parser;
