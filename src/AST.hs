{-# LANGUAGE GADTs #-}
module AST where

data Exp a where
  LitInt :: Int -> Exp Int
  Add    :: Exp Int -> Exp Int -> Exp Int
  Sub    :: Exp Int -> Exp Int -> Exp Int
  Mul    :: Exp Int -> Exp Int -> Exp Int
  Div    :: Exp Int -> Exp Int -> Exp Int
  
instance Show a => Show (Exp a) where
  show (LitInt x) = show x
  show (Add x y)  = show x ++ " + " ++ show y
  show (Sub x y)  = show x ++ " - " ++ show y
  show (Mul x y)  = show x ++ " * " ++ show y
  show (Div x y)  = show x ++ " / " ++ show y

eval :: Exp a -> a
eval (LitInt n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = if eval e2 == 0
                   then error "Cannot divide by 0"
                   else eval e1 `div` eval e2
