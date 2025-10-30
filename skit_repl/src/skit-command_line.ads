package Skit.Command_Line is

   function Load_Standard_Library return Boolean;
   function Trace_Evaluation return Boolean;
   function Trace_GC return Boolean;
   function Report return Boolean;
   function Core_Size return Natural;

   function Evaluate_Expression return String;

end Skit.Command_Line;
