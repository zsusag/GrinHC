module AST where

data Exp = EInt Int
  | EAdd Exp Exp
  | ESub Exp Exp
  | EMul Exp Exp
  | EDiv Exp Exp
  | EBool Bool
  | ELeq Exp Exp
  | EIf Exp Exp Exp

instance Show Exp where
  show (EInt n)       = show n
  show (EAdd e1 e2)   = show e1 ++ " + " ++ show e2
  show (ESub e1 e2)   = show e1 ++ " - " ++ show e2
  show (EMul e1 e2)   = show e1 ++ " * " ++ show e2
  show (EDiv e1 e2)   = show e1 ++ " / " ++ show e2
  show (EBool b)      = show b
  show (ELeq e1 e2)   = show e1 ++ " <= " ++ show e2
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2
                        ++ " else " ++ show e3
evaluate :: Exp -> String
evaluate e = let simp = simplify e
             in case simp of
                  EInt n  -> show n
                  EBool b -> show b
                  _       -> error "Incomplete evaluation: Unable to evaluate S-Expression"

simplify :: Exp -> Exp
simplify (EAdd e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) -> EInt $ n1 + n2
       _                  -> error ("Type Error: \"" ++ show (EAdd e1' e2') ++
                                    "\" does not typecheck.")
simplify (ESub e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) -> EInt $ n1 - n2
       _                  -> error ("Type Error: \"" ++ show (ESub e1' e2') ++
                             "\" does not typecheck.")
simplify (EMul e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) -> EInt $ n1 * n2
       _                  -> error ("Type Error: \"" ++ show (EMul e1' e2') ++
                             "\" does not typecheck.")
simplify (EDiv e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) -> if n2 /= 0
                             then EInt $ n1 `div` n2
                             else error "Divide by Zero: Cannot divide by zero"
       _                  -> error ("Type Error: \"" ++ show (EDiv e1' e2') ++
                             "\" does not typecheck.")
simplify (ELeq e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) -> EBool $ n1 <= n2
       _                  -> error ("Type Error: \"" ++ show (ELeq e1' e2') ++
                             "\" does not typecheck.")
simplify (EIf e1 e2 e3) = if b1 then simplify e2 else simplify e3
  where b1 = let e1' = simplify e1
             in case e1' of
                  (EBool b) -> b
                  _         -> error ("Type Error: " ++ show e1' ++
                                      " is not a boolean value within an \"if\" statement")
simplify e = e
