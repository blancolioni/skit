with Ada.Text_IO;
with Skit.Command_Line;
with Skit.Debug;
with Skit.Environment;
with Skit.Impl;
with Skit.Library;
with Skit.Machine;

procedure Skit_Repl is
   Machine : constant Skit.Machine.Reference :=
               Skit.Impl.Machine (Skit.Command_Line.Core_Size);
   Env : constant Skit.Environment.Reference :=
           Skit.Environment.Create
             (Machine);
begin
   if Skit.Command_Line.Load_Standard_Library then
      Skit.Library.Load_Standard_Library (Env);
   end if;

   if Skit.Command_Line.Trace_Evaluation then
      Machine.Set ("trace-eval", "true");
   end if;

   if Skit.Command_Line.Trace_GC then
      Machine.Set ("trace-gc", "true");
   end if;

   if Skit.Command_Line.Evaluate_Expression /= "" then
      Env.Evaluate (Skit.Command_Line.Evaluate_Expression);
      Ada.Text_IO.Put_Line
        (Skit.Debug.Image (Machine.Pop, Machine));
   else
      loop
         Ada.Text_IO.Put ("skit> ");
         Ada.Text_IO.Flush;
         declare
            Line : constant String := Ada.Text_IO.Get_Line;
         begin
            if Line = "" or else Line = ":quit" then
               exit;
            end if;

            if Line = ":report" then
               Machine.Report;
            else
               Env.Evaluate (Line);
               Ada.Text_IO.Put_Line
               (Skit.Debug.Image (Machine.Pop, Machine));
            end if;
         end;
      end loop;
   end if;
   if Skit.Command_Line.Report then
      Machine.Report;
   end if;
end Skit_Repl;
