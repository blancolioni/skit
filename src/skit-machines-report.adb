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
   Ada.Text_IO.Put_Line
     ("stack top" & This.Stacks (Stack).Top'Image
      & "; max" & This.Stacks (Stack).Max'Image);
   Ada.Text_IO.Put_Line
     ("control top" & This.Stacks (Control).Top'Image
      & "; max" & This.Stacks (Control).Max'Image);
   Ada.Text_IO.Put_Line
     ("stash top" & This.Stacks (Secondary_Stack).Top'Image
      & "; max" & This.Stacks (Secondary_Stack).Max'Image);

   Skit.Memory.Report (This.Core);
end Skit.Machines.Report;
