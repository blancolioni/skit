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
      & This.Stats.Alloc_Count'Image);
   Ada.Text_IO.Put_Line
     ("Reclaimed cells:"
      & This.Stats.Reclaimed'Image);
   Ada.Text_IO.Put_Line
     ("Last copied cells:"
      & This.Stats.Copied'Image
      & " static:"
      & This.Stats.Static_Copied'Image
      & "; transient:"
      & This.Stats.Transient_Copied'Image);
   Ada.Text_IO.Put_Line
     ("Static<-young writes: total"
      & This.Stats.Remembered_Writes'Image
      & "  max/epoch:"
      & This.Stats.Max_Remembered'Image);
end Skit.Memory.Report;
