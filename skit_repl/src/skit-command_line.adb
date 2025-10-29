package body Skit.Command_Line is

   function Load_Standard_Library return Boolean is (True);
   function Trace_Evaluation return Boolean is (True);

   function Evaluate_Expression return String
   is ("(\x.+ x x) ((\x.trace x) 2)");

end Skit.Command_Line;
