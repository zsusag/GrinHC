module Token where

data Token
  = TokenLParen
  | TokenRParen
  | TokenInt Int
  | TokenPlus
  | TokenSub
  | TokenMult
  | TokenDiv
  | TokenBool Bool
  | TokenLEQ
  | TokenIf
  | TokenFloat Float
  | TokenNaN
  deriving (Show)
