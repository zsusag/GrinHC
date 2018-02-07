module AST where

data Exp = EInt Int
  | EAdd Exp Exp
  | ESub Exp Exp
  | EMul Exp Exp
  | EDiv Exp Exp
  | EBool Bool
  | ELeq Exp Exp
  | EIf Exp Exp Exp
  | EFloat Float
  | ENaN
  
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
  show (EFloat f)     = show f
  show ENaN           = "NaN"

evaluate :: Exp -> String
evaluate e = show simp
  where simp = simplify e

simplify :: Exp -> Exp
simplify (EAdd e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2)     -> EInt $ n1 + n2
       (EFloat n1, EInt n2)   -> EFloat $ n1 + fromIntegral n2
       (EInt n1, EFloat n2)   -> EFloat $ fromIntegral n1 + n2
       (EFloat n1, EFloat n2) -> EFloat $ n1 + n2
       (ENaN, _)              -> ENaN
       (_, ENaN)              -> ENaN
       _                      -> errorWithoutStackTrace ("Type Error: \"" ++ show (EAdd e1' e2') ++
                                                         "\" does not typecheck.")
simplify (ESub e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2)     -> EInt $ n1 - n2
       (EFloat n1, EInt n2)   -> EFloat $ n1 - fromIntegral n2
       (EInt n1, EFloat n2)   -> EFloat $ fromIntegral n1 - n2
       (EFloat n1, EFloat n2) -> EFloat $ n1 - n2
       (ENaN, _)              -> ENaN
       (_, ENaN)              -> ENaN
       _                  -> errorWithoutStackTrace ("Type Error: \"" ++ show (ESub e1' e2') ++
                                                     "\" does not typecheck.")
simplify (EMul e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) -> EInt $ n1 * n2
       (EFloat n1, EInt n2)   -> EFloat $ n1 * fromIntegral n2
       (EInt n1, EFloat n2)   -> EFloat $ fromIntegral n1 * n2
       (EFloat n1, EFloat n2) -> EFloat $ n1 * n2
       (ENaN, _)              -> ENaN
       (_, ENaN)              -> ENaN
       _                  -> errorWithoutStackTrace ("Type Error: \"" ++ show (EMul e1' e2') ++
                                                     "\" does not typecheck.")
simplify (EDiv e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) | n2 /= 0 -> EInt $ n1 `div` n2
                          | n1 == 0 -> ENaN
                          | otherwise -> errorWithoutStackTrace "Divide By Zero: Cannot divide by Zero"
       (EFloat n1, EInt n2)   -> EFloat $ n1 / fromIntegral n2
       (EInt n1, EFloat n2)   -> EFloat $ fromIntegral n1 / n2
       (EFloat n1, EFloat n2) -> EFloat $ n1 / n2
       (ENaN, _)              -> ENaN
       (_, ENaN)              -> ENaN
       _                  -> errorWithoutStackTrace ("Type Error: \"" ++ show (EDiv e1' e2') ++
                                                     "\" does not typecheck.")
simplify (ELeq e1 e2) = let e1' = simplify e1
                            e2' = simplify e2
  in case (e1',e2') of
       (EInt n1, EInt n2) -> EBool $ n1 <= n2
       (EFloat n1, EInt n2)   -> EBool $ n1 <= fromIntegral n2
       (EInt n1, EFloat n2)   -> EBool $ fromIntegral n1 <= n2
       (EFloat n1, EFloat n2) -> EBool $ n1 <= n2
       (ENaN, _)              -> EBool False
       (_, ENaN)              -> EBool False
       _                  -> errorWithoutStackTrace ("Type Error: \"" ++ show (ELeq e1' e2') ++
                                                     "\" does not typecheck.")
simplify (EIf e1 e2 e3) = if b1 then simplify e2 else simplify e3
  where b1 = let e1' = simplify e1
             in case e1' of
                  (EBool b) -> b
                  _         -> errorWithoutStackTrace ("Type Error: " ++ show e1' ++
                                                       " is not a boolean value within an \"if\" statement")
simplify e = e
