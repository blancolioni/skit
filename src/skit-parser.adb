package body Skit.Parser is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Source    : String;
      To_Object : not null access
        function (X : String) return Object;
      Machine   : not null access Skit.Machine.Abstraction'Class)
   is
      Index : Natural := Source'First - 1;
      Ch    : Character := ' ';

      function At_End return Boolean
      is (Index > Source'Last);

      function At_Expression return Boolean
      is (not At_End and then Ch /= ')');

      procedure Skip;

      procedure Skip_Spaces;

      procedure Parse_Expression;
      procedure Parse_Atomic;

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
         else
            declare
               Id : String (1 .. 200);
               Last : Natural := 0;
            begin
               while Ch /= ' ' loop
                  Last := Last + 1;
                  Id (Last) := Ch;
                  Skip;
               end loop;
               declare
                  Tok : constant String := Id (1 .. Last);
               begin
                  Machine.Push (To_Object (Tok));
               end;
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

      Parse_Expression;

   end Parse;

end Skit.Parser;
