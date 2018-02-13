{
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Lexer (
  Token(..),
  alexScanTokens,
  AlexPosn(..),
  tokenPosition
  ) where

import Lang
}

%wrapper "posn"

$digit = 0-9

tokens :-

  $white+ ;
  \(      { tok (\p s -> TokenLParen p) }
  \)      { tok (\p s -> TokenRParen p) }
  \+      { tok (\p s -> TokenPlus p) }
  \-      { tok (\p s -> TokenSub p) }
  \*      { tok (\p s -> TokenMult p) }  
  \/      { tok (\p s -> TokenDiv p) }
  "<="    { tok (\p s -> TokenLEQ p) }
  true    { tok (\p s -> TokenBool p True) }
  false   { tok (\p s -> TokenBool p False) }
  if      { tok (\p s -> TokenIf p) }
  NaN     { tok (\p s -> TokenNaN p) }
  $digit+ { tok (\p s -> TokenInt p (read s)) }
  $digit+\.$digit+  { (\p s -> TokenFloat p (read s)) }
{
-- Some action helpers:
tok f p s = f p s

data Token
  = TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenInt AlexPosn Int
  | TokenPlus AlexPosn
  | TokenSub AlexPosn
  | TokenMult AlexPosn
  | TokenDiv AlexPosn
  | TokenBool AlexPosn Bool
  | TokenLEQ AlexPosn
  | TokenIf AlexPosn
  | TokenFloat AlexPosn Float
  | TokenNaN AlexPosn

instance Show Token where
  show (TokenLParen _) = "("
  show (TokenRParen _) = ")"
  show (TokenInt _ n)  = show n
  show (TokenPlus _)   = "+"
  show (TokenSub _)    = "-"
  show (TokenMult _)   = "*"
  show (TokenDiv _)    = "/"
  show (TokenBool _ True) = "true"
  show (TokenBool _ False) = "false"
  show (TokenLEQ _)    = "<="
  show (TokenIf _)     = "if"
  show (TokenFloat _ f) = show f
  show (TokenNaN _)    = "NaN"

tokenPosn :: Token -> AlexPosn
tokenPosn (TokenLParen p)  = p
tokenPosn (TokenRParen p)  = p
tokenPosn (TokenInt p _)   = p
tokenPosn (TokenPlus p)    = p
tokenPosn (TokenSub p)     = p
tokenPosn (TokenMult p)    = p
tokenPosn (TokenDiv p)     = p
tokenPosn (TokenBool p _)  = p
tokenPosn (TokenLEQ p)     = p
tokenPosn (TokenIf p)      = p
tokenPosn (TokenFloat p _) = p
tokenPosn (TokenNaN p)     = p

tokenPosition :: Token -> Pos
tokenPosition (TokenLParen (AlexPn _ line col))   = (line,col)
tokenPosition (TokenRParen (AlexPn _ line col))   = (line,col)
tokenPosition (TokenInt (AlexPn _ line col) _ )   = (line,col)
tokenPosition (TokenPlus (AlexPn _ line col))     = (line,col)
tokenPosition (TokenSub (AlexPn _ line col))      = (line,col)
tokenPosition (TokenMult (AlexPn _ line col))     = (line,col)
tokenPosition (TokenDiv (AlexPn _ line col))      = (line,col)
tokenPosition (TokenBool (AlexPn _ line col) _ )  = (line,col)
tokenPosition (TokenLEQ (AlexPn _ line col))      = (line,col)
tokenPosition (TokenIf (AlexPn _ line col))       = (line,col)
tokenPosition (TokenFloat (AlexPn _ line col) _ ) = (line,col)
tokenPosition (TokenNaN (AlexPn _ line col))      = (line,col)
}
