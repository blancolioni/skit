with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;

package body Skit.Debug is

   type Variable_Binding is
      record
         Name : Character;
         V    : Object;
      end record;

   package Variable_Binding_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Variable_Binding);

   -----------
   -- Image --
   -----------

   function Image (X : Object) return String is
   begin
      if Is_Integer (X) then
         return Ada.Strings.Fixed.Trim
           (To_Integer (X)'Image, Ada.Strings.Left);
      elsif Is_Float (X) then
         return Ada.Strings.Fixed.Trim
           (To_Float (X)'Image, Ada.Strings.Left);
      elsif Is_Application (X) then
         return "("
           & Ada.Strings.Fixed.Trim (Address (X)'Image, Ada.Strings.Left)
           & ")";
      elsif Is_Combinator (X) then
         return (case Combinator_Of (X) is
                    when Comb_S       => "S",
                    when Comb_K       => "K",
                    when Comb_I       => "I",
                    when Comb_C       => "C",
                    when Comb_B       => "B",
                    when Comb_S_Prime => "S'",
                    when Comb_B_Star  => "B*",
                    when Comb_C_Prime => "C'",
                    when Comb_Y       => "Y");
      elsif X = Nil then
         return "nil";
      elsif X = Undefined then
         return "*undefined*";
      elsif X = Suspension then
         return "*suspend*";
      elsif Is_Symbol (X) then
         return [Character'Val (Symbol_Index (X) + Character'Pos ('a'))];
      elsif Is_Primitive_Function (X) then
         return "<prim"
           & Natural'Image (Primitive_Function_Index (X)) & ">";
      elsif Is_Foreign_Object (X) then
         return "<foreign"
           & Natural'Image (Foreign_Object_Index (X)) & ">";
      else
         return "<?>";
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (X    : Object;
      Core : Skit.Memory.Instance)
      return String
   is
      use Skit.Memory;

      Vrbs : Variable_Binding_Lists.List;
      Xs   : constant String := "xyzuvwijkabcdefghlmnopqrst";

      package Address_Sets is
        new Ada.Containers.Ordered_Sets (Cell_Address);

      Visited_Set : Address_Sets.Set;

      function Img (X : Object) return String;

      ---------
      -- Img --
      ---------

      function Img (X : Object) return String is
      begin
         if Is_Application (X) then
            if Visited_Set.Contains (Address (X)) then
               return "[recursive]";
            end if;
            Visited_Set.Include (Address (X));
            declare
               Left_Img  : constant String := Img (Left (Core, X));
               Right_Img : constant String := Img (Right (Core, X));
            begin
               if Is_Application (Right (Core, X)) then
                  return Left_Img & " (" & Right_Img & ")";
               else
                  return Left_Img & " " & Right_Img;
               end if;
            end;
         elsif False
           and then Is_Symbol (X)
         then
            for Binding of Vrbs loop
               if Binding.V = X then
                  return [Binding.Name];
               end if;
            end loop;
            Vrbs.Append (Variable_Binding'(Xs (Natural (Vrbs.Length) + 1), X));
            return [Vrbs.Last_Element.Name];
         else
            return Image (X);
         end if;
      end Img;

   begin
      return Img (X);
   end Image;

end Skit.Debug;
