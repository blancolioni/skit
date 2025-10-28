with Skit.Tests;

procedure Skit_Tests is
   use Skit.Tests;
   Plus : constant Stack_Operation_Type :=
            Prim (2);
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
         [Plus, I, Int (1), Apply, Apply, Int (2), Apply],
         Skit.To_Object (3));
   Test_Compiler ("+ 1 2 ==> 3",
         [Plus, Int (1), Apply, Int (2), Apply],
         Skit.To_Object (3));
   Test_Compiler
     ("\x.x ==> I",
      [Lambda, Var (0), Var (0), Apply, Apply],
      Skit.I);
   Test_Compiler
     ("(\x.+ x x) 5 ==> 10",
      [Lambda, Var (0), Plus, Var (0), Apply, Var (0), Apply, Apply, Apply,
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
   Test ("le 1 2 3 4", 3);
   Test ("le 2 2 3 4", 3);
   Test ("le 3 2 3 4", 4);
   Test ("lt 1 2 3 4", 3);
   Test ("lt 2 2 3 4", 4);
   Test ("lt 3 2 3 4", 4);
   Test ("ge 1 2 3 4", 4);
   Test ("ge 2 2 3 4", 3);
   Test ("ge 3 2 3 4", 3);
   Test ("gt 1 2 3 4", 4);
   Test ("gt 2 2 3 4", 4);
   Test ("gt 3 2 3 4", 3);
   Test ("eq 1 2 3 4", 4);
   Test ("eq 2 2 3 4", 3);
   Test ("eq 3 2 3 4", 4);
   Test ("ne 1 2 3 4", 3);
   Test ("ne 2 2 3 4", 4);
   Test ("ne 3 2 3 4", 3);
   Test ("(\x.eq 0 x true 3) 0 2 3", 2);
   Test ("(\x.eq 0 x 2 3) 1", 3);
   Test ("false 1 2", 2);
   Test ("true 1 2", 1);
   Test ("(\x.eq x 0 true false) 0 2 3", 2);
   Test ("true", Skit.K);
   Test ("succ 41", 42);
   Test ("pred 8088", 8087);
   Test ("* 4 (* 3 (* 2 (* 1 1)))", 24);
   Test ("(\x.+ x x)(+ 1 2)", 6);
   Test ("fac 1", 1);
   Test ("gcd 1 1", 1);
   Test ("fac 2", 2);
   Test ("fac 3", 6);
   Test ("fac 5", 120);
   Test ("gcd 35 5", 5);
   Test ("gcd 122 12", 2);
   Test ("putChar 1 1 955", 2);
   Test ("head str", 66);
   Test ("head (tail str)", 67);
   Test ("null nil 1 2", 1);
   Test ("null str 1 2", 2);
   Test ("null (cons 13 14) 1 2", 2);
   Test ("sum (cons 1 (cons 2 (cons 3 nil)))", 6);
   Test ("sum (map (* 2) (cons 1 (cons 2 (cons 3 nil))))", 12);
   Test ("sum (map (putChar 1 1) (cons 65 (cons 66 (cons 67 (cons 10 nil)))))",
         8);
   Report;
end Skit_Tests;
