with Ada.Text_IO;

procedure Skit.Machines.Report
  (This : Instance'Class)
is
begin
   Ada.Text_IO.Put_Line
     ("Evaluation:"
      & Natural'Image (Natural (This.Eval_Time * 1000.0))
      & "ms");
   Ada.Text_IO.Put_Line
     ("GC:"
      & Natural'Image (This.GC_Count)
      & " @"
      & Natural'Image (Natural (This.GC_Time * 1000.0))
      & "ms");
   Ada.Text_IO.Put_Line
     ("Allocated cells:"
      & Natural'Image (This.Total_Alloc_Count));
   Ada.Text_IO.Put_Line
     ("Reclaimed cells:"
      & Natural'Image (This.Reclaimed));
end Skit.Machines.Report;
