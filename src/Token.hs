module Token where

data Token
  = TokenLParen
  | TokenRParen
  | TokenInt Int
  | TokenPlus
  | TokenSub
  | TokenMult
  | TokenDiv
  deriving (Show)
