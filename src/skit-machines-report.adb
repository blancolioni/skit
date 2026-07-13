with Ada.Text_IO;
with Skit.Memory.Report;

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
   Skit.Memory.Report (This.Core);
end Skit.Machines.Report;
