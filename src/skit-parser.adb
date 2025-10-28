with Skit.Compiler;

package body Skit.Parser is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Source    : String;
      To_Object : not null access
        function (X : String) return Object;
      Bind_Value : not null access
        procedure (Name : String);
      Machine   : not null access Skit.Machine.Abstraction'Class)
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

      procedure Parse_Expression;
      procedure Parse_Atomic;
      function Parse_Id return String;

      ------------------
      -- Parse_Atomic --
      ------------------

      procedure Parse_Atomic is
      begin
         if Ch = '(' then
            Skip;
            Parse_Expression;
            if Ch = ')' then
               Skip;
            else
               raise Parse_Error with
                 "Position" & Index'Image & ": expected ')'";
            end if;
         elsif Ch = '\' then
            Skip;
            declare
               X : constant String := Parse_Id;
            begin
               Skip_Spaces;
               if Ch = '.' then
                  Skip;
               end if;
               Machine.Push (Skit.λ);
               Machine.Push (To_Object (X));
               Parse_Expression;
               Machine.Apply;
               Machine.Apply;
            end;
         else
            declare
               Id : constant String := Parse_Id;
            begin
               Machine.Push (To_Object (Id));
            end;
         end if;
      end Parse_Atomic;

      ----------------------
      -- Parse_Expression --
      ----------------------

      procedure Parse_Expression is
      begin
         Skip_Spaces;
         if At_End then
            return;
         end if;

         Parse_Atomic;

         Skip_Spaces;
         while At_Expression loop
            Parse_Atomic;
            Machine.Apply;
            Skip_Spaces;
         end loop;
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
         begin
            Parse_Expression;
            Skit.Compiler.Compile (Machine);
            Bind_Value (Name);
         end;
      else
         Parse_Expression;
      end if;
   end Parse;

end Skit.Parser;
