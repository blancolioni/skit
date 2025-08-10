with Skit.Tests;

procedure Skit_Tests is
   use Skit.Tests;
begin
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

end Skit_Tests;
