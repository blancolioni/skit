with Ada.Text_IO;
procedure Skit.Memory.Report
  (This : Instance)
is
begin
   Ada.Text_IO.Put_Line
     ("Core size:"
      & This.Space_Size'Image);
   Ada.Text_IO.Put_Line
     ("Static size:"
      & Cell_Address'Image (This.Static_Top - This.Static_Space));
   Ada.Text_IO.Put_Line
     ("Static cells:"
      & Cell_Address'Image (This.Static_Free - This.Static_Space));
   Ada.Text_IO.Put_Line
     ("Allocated cells:"
      & Natural'Image (This.Alloc_Count));
   Ada.Text_IO.Put_Line
     ("Reclaimed cells:"
      & Natural'Image (This.Reclaimed));
   Ada.Text_IO.Put_Line ("to-space:" & This.To_Space'Image
                         & This.Free'Image & This.Top'Image);

   Ada.Text_IO.Put_Line
     ("Active cells:"
      & Cell_Address'Image (This.Free - This.To_Space));
   Ada.Text_IO.Put_Line
     ("Available cells:"
      & Cell_Address'Image (This.Top - This.Free));
   Ada.Text_IO.Put_Line
     ("Total cells:"
      & Cell_Address'Image (This.Top - This.To_Space));
   Ada.Text_IO.Put_Line
     ("Last copied cells:"
      & This.Copied'Image
      & " static:"
      & This.Static_Copied'Image
      & "; transient:"
      & This.Transient_Copied'Image);
   Ada.Text_IO.Put_Line
     ("Static<-young writes: total"
      & This.Remembered_Writes'Image
      & "  max/epoch:"
      & This.Max_Remembered'Image);
end Skit.Memory.Report;
