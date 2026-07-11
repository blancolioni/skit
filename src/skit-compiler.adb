package body Skit.Compiler is

   function Is_S_Combinator (T : Skit.Terms.Term) return Boolean;

   function Abstract_Variable
     (Variable : String;
      Top      : Skit.Terms.Term)
      return Skit.Terms.Term;

   function Optimise
     (Top      : Skit.Terms.Term)
      return Skit.Terms.Term
      with Pre => Is_S_Combinator (Top);

   -----------------------
   -- Abstract_Variable --
   -----------------------

   function Abstract_Variable
     (Variable : String;
      Top      : Skit.Terms.Term)
      return Skit.Terms.Term
   is
      use Skit.Terms;
   begin
      if Skit.Terms.Is_Application (Top) then
         return Optimise
           (Apply
              (Apply
                   (Combinator (Skit.S),
                    Abstract_Variable (Variable, Get_Left (Top))),
            Abstract_Variable (Variable, Get_Right (Top))));
      elsif Is_Symbol (Top)
        and then Get_Symbol (Top) = Variable
      then
         return Combinator (Skit.I);
      else
         return Apply (Combinator (Skit.K), Top);
      end if;
   end Abstract_Variable;

   -------------
   -- Compile --
   -------------

   function Compile
     (E : Skit.Terms.Term)
      return Skit.Terms.Term
   is
      use Skit.Terms;
      Result : constant Skit.Terms.Term :=
                 (if Is_Lambda (E)
                  then Abstract_Variable
                    (Get_Variable (E),
                     Compile (Get_Body (E)))
                  elsif Is_Application (E)
                  then Apply
                    (Compile (Get_Left (E)), Compile (Get_Right (E)))
                  else E);
   begin
      return Result;
   end Compile;

   ---------------------
   -- Is_S_Combinator --
   ---------------------

   function Is_S_Combinator (T : Skit.Terms.Term) return Boolean is
      use Skit.Terms;
   begin
      return Is_Application (T)
          and then Is_Application (Get_Left (T))
          and then Is_Combinator (Get_Left (Get_Left (T)), Skit.S);
   end Is_S_Combinator;

   --------------
   -- Optimise --
   --------------

   function Optimise
     (Top      : Skit.Terms.Term)
      return Skit.Terms.Term
   is
      use Skit.Terms;

      function Is_App_K (T : Term) return Boolean
      is (Is_Application (T)
          and then Is_Combinator (Get_Left (T), Skit.K));

      function App_K (T : Term) return Term
      is (Get_Right (T));

      function Is_App_B (T : Term) return Boolean
      is (Is_Application (T)
          and then Is_Application (Get_Left (T))
          and then Is_Combinator (Get_Left (Get_Left (T)), Skit.B));

      function App_B_Q (T : Term) return Term
      is (Get_Right (Get_Left (T)));

      function App_B_R (T : Term) return Term
      is (Get_Right (T));

      X : constant Term := Get_Right (Get_Left (Top));
      Y : constant Term := Get_Right (Top);

   begin
      if Is_App_K (X) then
         if Is_App_K (Y) then
            return Apply (Combinator (Skit.K),
                           Apply (App_K (X), App_K (Y)));
         elsif Is_Combinator (Y, Skit.I) then
            return App_K (X);
         elsif Is_App_B (Y) then
            return Apply
               (Apply
                  (Apply
                        (Combinator (Skit.B_Star),
                        App_K (X)),
                  App_B_Q (Y)),
               App_B_R (Y));
         else
            return Apply
               (Apply
                  (Combinator (Skit.B),
                  App_K (X)),
               Y);
         end if;
      elsif Is_App_K (Y) then
         if Is_App_B (X) then
            return Apply
               (Apply
                  (Apply
                        (Combinator (Skit.C_Prime),
                        App_B_Q (X)),
                  App_B_R (X)),
               App_K (Y));
         else
            return Apply
               (Apply
                  (Combinator (Skit.C), X),
               App_K (Y));
         end if;
      elsif Is_App_B (X) then
         return Apply
            (Apply
               (Apply
                     (Combinator (Skit.S_Prime),
                     App_B_Q (X)),
               App_B_R (X)),
            Y);
      else
         return Top;
      end if;
   end Optimise;

end Skit.Compiler;
