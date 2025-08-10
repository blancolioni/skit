with Skit.Stacks;

package Skit.Evaluator is

   type Abstraction is interface and Skit.Stacks.Abstraction;

   procedure Evaluate
     (This : in out Abstraction)
   is abstract;

end Skit.Evaluator;
