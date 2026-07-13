with Ada.Text_IO;
procedure Skit.Memory.Report
  (This : Instance)
is
begin
   Ada.Text_IO.Put_Line
     ("Core size:"
      & This.Space_Size'Image);
   Ada.Text_IO.Put_Line
     ("Allocated cells:"
      & Natural'Image (This.Alloc_Count));
   Ada.Text_IO.Put_Line
     ("Reclaimed cells:"
      & Natural'Image (This.Reclaimed));
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
