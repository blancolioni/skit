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
      case Tag (X) is
         when Integer_Object =>
            return Ada.Strings.Fixed.Trim
              (To_Integer (X)'Image, Ada.Strings.Left);
         when Float_Object =>
            return Ada.Strings.Fixed.Trim
              (To_Float (X)'Image, Ada.Strings.Left);
         when Primitive_Object =>
            case Payload (X) is
               when Payload_Nil =>
                  return "nil";
               when Payload_S =>
                  return "S";
               when Payload_K =>
                  return "K";
               when Payload_I =>
                  return "I";
               when Payload_C =>
                  return "C";
               when Payload_B =>
                  return "B";
               when Payload_S_Prime =>
                  return "S'";
               when Payload_B_Star =>
                  return "B*";
               when Payload_C_Prime =>
                  return "C'";
               when Payload_Undefined =>
                  return "*undefined*";
               when Payload_Suspension =>
                  return "*suspend*";
               when Primitive_Variable_Payload =>
                  declare
                     Ch : constant Character :=
                            Character'Val
                              (Payload (X) - Primitive_Variable_Payload'First
                               + Character'Pos ('a'));
                  begin
                     return [Ch];
                  end;
               when others =>
                  return "<"
                    & Ada.Strings.Fixed.Trim
                    (Payload (X)'Image, Ada.Strings.Left)
                    & ">";
            end case;
         when Application_Object =>
            return "("
              & Ada.Strings.Fixed.Trim (Payload (X)'Image, Ada.Strings.Left)
              & ")";
      end case;
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

      function Img (X : Object) return String;

      ---------
      -- Img --
      ---------

      function Img (X : Object) return String is
      begin
         if Tag (X) = Application_Object then
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
           and then Tag (X) = Primitive_Object
           and then Payload (X) in Primitive_Variable_Payload
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
