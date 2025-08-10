with Ada.Strings.Fixed;

package body Skit.Debug is

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
                  return "x";
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
         else
            return Image (X);
         end if;
      end Img;

   begin
      return Img (X);
   end Image;

end Skit.Debug;
