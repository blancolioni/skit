with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

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
      case X.Tag is
         when Integer_Object =>
            return Ada.Strings.Fixed.Trim
              (To_Integer (X)'Image, Ada.Strings.Left);
         when Float_Object =>
            return Ada.Strings.Fixed.Trim
              (To_Float (X)'Image, Ada.Strings.Left);
         when Primitive_Object =>
            case X.Payload is
               when 0 =>
                  return "nil";
               when 1 =>
                  return "S";
               when 2 =>
                  return "K";
               when 3 =>
                  return "I";
               when 4 =>
                  return "C";
               when 5 =>
                  return "B";
               when 6 =>
                  return "\";
               when Primitive_Variable_Payload =>
                  declare
                     S : String :=
                           Object_Payload'Image
                             (X.Payload - Primitive_Variable_Payload'First);
                  begin
                     S (1) := '_';
                     return S;
                  end;
               when others =>
                  return "<"
                    & Ada.Strings.Fixed.Trim
                    (X.Payload'Image, Ada.Strings.Left)
                    & ">";
            end case;
         when Application_Object =>
            return "("
              & Ada.Strings.Fixed.Trim (X.Payload'Image, Ada.Strings.Left)
              & ")";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (X    : Object;
      Core : not null access constant Skit.Memory.Abstraction'Class)
      return String
   is

      Vrbs : Variable_Binding_Lists.List;
      Xs   : constant String := "xyzuvwijkabcdefghlmnopqrst";

      function Img (X : Object) return String;

      ---------
      -- Img --
      ---------

      function Img (X : Object) return String is
      begin
         if X.Tag = Application_Object then
            if Core.Left (X) = λ then
               return "(\" & Img (Core.Left (Core.Right (X)))
                 & "." & Img (Core.Right (Core.Right (X)))
                 & ")";
            end if;
            declare
               Left_Img  : constant String := Img (Core.Left (X));
               Right_Img : constant String := Img (Core.Right (X));
            begin
               if Core.Right (X).Tag = Application_Object then
                  return Left_Img & " (" & Right_Img & ")";
               else
                  return Left_Img & " " & Right_Img;
               end if;
            end;
         elsif False
           and then X.Tag = Primitive_Object
           and then X.Payload in Primitive_Variable_Payload
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
