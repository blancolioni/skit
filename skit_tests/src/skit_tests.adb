with Skit.Tests;

procedure Skit_Tests is
   use Skit.Tests;
begin
   Initialize;
   Test ("I x ==> x", [I, Int (1), Apply], Skit.To_Object (1));
   Test ("K x y ==> x", [K, Int (42), Apply, Int (666), Apply],
         Skit.To_Object (42));
   Test ("S K S K ==> K",
         [S, K, Apply, S, Apply, K, Apply],
         Skit.K);
   Test ("S K K x ==> x",
         [S, K, Apply, K, Apply, Int (42), Apply],
         Skit.To_Object (42));
   Test ("SKI(KIS) ==> I",
         [S, K, Apply, I, Apply, K, I, Apply, S, Apply, Apply],
         Skit.I);
   Test ("KS(I(SKSI)) ==> S",
         [K, S, Apply, I, S, K, Apply, S, Apply, I, Apply, Apply, Apply],
         Skit.S);
   Test ("+ (I 1) 2 ==> 3",
         [Prim (0), I, Int (1), Apply, Apply, Int (2), Apply],
         Skit.To_Object (3));
   Test_Compiler ("+ 1 2 ==> 3",
         [Prim (0), Int (1), Apply, Int (2), Apply],
         Skit.To_Object (3));
   Test_Compiler
     ("\x.x ==> I",
      [Λ, Var (0), Var (0), Apply, Apply],
      Skit.I);
   Test_Compiler
     ("(\x.+ x x) 5 ==> 10",
      [Λ, Var (0), Prim (0), Var (0), Apply, Var (0), Apply, Apply, Apply,
       Int (5), Apply],
      Skit.To_Object (10));
   Test ("1", Skit.To_Object (1));
   Test ("+ 1 1", Skit.To_Object (2));
   Test ("- 2 1", Skit.To_Object (1));
   Test ("- 1 2", Skit.To_Object (-1));
   Test ("(\x.+ x 40) 2", Skit.To_Object (42));
   Test ("id 10", Skit.To_Object (10));
   Test ("if true 1 2", Skit.To_Object (1));
   Test ("if false 1 2", Skit.To_Object (2));
   Test ("succ 12", 13);
   Test ("zero 1 2 3", 3);
   Test ("zero 0 2 3", 2);
   Test ("eq 0 1 2 3", 3);
   Test ("eq 0 0 2 3", 2);
   Test ("(\x.eq 0 x true 3) 0 2 3", 2);
   Test ("(\x.eq 0 x 2 3) 1", 3);
   Test ("false 1 2", 2);
   Test ("true 1 2", 1);
   Test ("(\x.eq x 0 true false) 0 2 3", 2);
   Test ("true", Skit.K);
   Test ("* 4 (* 3 (* 2 (* 1 1)))", 24);
   Test ("(\x.+ x x)(+ 1 2)", 6);
   Test ("fac 1", 1);
   Test ("gcd 1 1", 1);
   Test ("fac 2", 2);
   Test ("fac 3", 6);
   Test ("fac 5", 120);
   Test ("gcd 35 5", 5);
   Test ("gcd 122 12", 2);
   Report;
end Skit_Tests;
