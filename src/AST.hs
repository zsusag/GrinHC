{-# LANGUAGE GADTs #-}
module AST where

data Exp a where
  LitInt :: Int -> Exp Int
  Add    :: Exp Int -> Exp Int -> Exp Int

instance Show a => Show (Exp a) where
  show (LitInt x) = show x
  show (Add x y)  = "+ " ++ show x ++ " " ++ show y

eval :: Exp a -> a
eval (LitInt n) = n
eval (Add e1 e2) = eval e1 + eval e2
