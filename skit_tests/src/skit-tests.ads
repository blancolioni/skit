with Skit.Terms;

package Skit.Tests is

   type Stack_Operation_Type is private;

   function Push (X : Skit.Terms.Term) return Stack_Operation_Type;
   function Apply return Stack_Operation_Type;

   function I return Stack_Operation_Type;
   function S return Stack_Operation_Type;
   function K return Stack_Operation_Type;
   function B return Stack_Operation_Type;
   function C return Stack_Operation_Type;
   function Lambda return Stack_Operation_Type;

   function Int (X : Integer) return Stack_Operation_Type;
   function Prim (P : Natural) return Stack_Operation_Type;
   function Var (V : String) return Stack_Operation_Type;

   type Stack_Operation_Array is
     array (Positive range <>) of Stack_Operation_Type;

   procedure Initialize;

   procedure Test
     (Name       : String;
      Operations : Stack_Operation_Array;
      Expected   : Object);

   procedure Test
     (Source   : String;
      Expected : Object);

   procedure Test
     (Source   : String;
      Expected : Integer);

   procedure Test
     (Source   : String;
      Expected : String);

   procedure Test_Foreign_Objects;
   --  Exercise the foreign-object registry and its GC: pinning, live-cell
   --  discovery, bare-root marking, child forwarding, sweeping the
   --  unreachable, and slot reuse.

   procedure Test_Foreign_Nested;
   --  A foreign object reachable only through another object's child cell must
   --  be found by the discovery fixpoint, and collected once the outer object
   --  becomes unreachable.

   procedure Test_Images;
   --  Round-trip a module image: write a graph to disk, read it back into a
   --  fresh machine, and check its structure survived relocation.

   procedure Report;

private

   type Op_Type is (Push, Apply, Lambda);

   type Stack_Operation_Type is
      record
         Op : Op_Type;
         X  : Skit.Terms.Term;
      end record;

   function Push (X : Skit.Terms.Term) return Stack_Operation_Type
   is (Push, X);

   function Apply return Stack_Operation_Type
   is (Apply, Skit.Terms.Primitive (Nil));

   function Push (X : Object) return Stack_Operation_Type
   is (Push (Skit.Terms.Primitive (X)));

   function I return Stack_Operation_Type is (Push (Skit.I));
   function S return Stack_Operation_Type is (Push (Skit.S));
   function K return Stack_Operation_Type is (Push (Skit.K));
   function B return Stack_Operation_Type is (Push (Skit.B));
   function C return Stack_Operation_Type is (Push (Skit.C));
   function Lambda return Stack_Operation_Type
   is (Lambda, Skit.Terms.Primitive (Nil));

   function Int (X : Integer) return Stack_Operation_Type
   is (Push (Skit.Terms.Const (X)));

end Skit.Tests;
