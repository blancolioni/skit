package body Skit.Command_Line is

   function Load_Standard_Library return Boolean is (True);
   function Trace_Evaluation return Boolean is (False);
   function Trace_GC return Boolean is (True);
   function Report return Boolean is (True);
   function Core_Size return Natural is (128 * 1024);
   function Evaluate_Expression return String
   is ("sum (fromTo 1 1000)");
   --  is ("(\x.+ x x) ((\x.trace x) 2)");

end Skit.Command_Line;
