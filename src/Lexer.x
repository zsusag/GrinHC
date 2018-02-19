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
$upper = [A-Z]
$lower = [a-z \_]

$idchar  = [$lower $upper $digit \']

@varid = $lower $idchar*

tokens :-

  $white+           ;
  \(                { tok (\p s -> TokenLParen p) }
  \)                { tok (\p s -> TokenRParen p) }
  \+                { tok (\p s -> TokenPlus p) }
  \-                { tok (\p s -> TokenSub p) }
  \*                { tok (\p s -> TokenMult p) }  
  \/                { tok (\p s -> TokenDiv p) }
  "<="              { tok (\p s -> TokenLte p) }
  ">="              { tok (\p s -> TokenGeq p) }
  "=="              { tok (\p s -> TokenEq p) }
  true              { tok (\p s -> TokenBool p True) }
  false             { tok (\p s -> TokenBool p False) }
  if                { tok (\p s -> TokenIf p) }
  then              { tok (\p s -> TokenThen p) }
  else              { tok (\p s -> TokenElse p) }
  NaN               { tok (\p s -> TokenNaN p) }
  $digit+           { tok (\p s -> TokenInt p (read s)) }
  $digit+\.$digit+  { (\p s -> TokenFloat p (read s)) }
  @varid            { tok (\p s -> TokenLid p s) }
{
-- Some action helpers:
tok f p s = f p s

data Token
  = TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenInt AlexPosn !Int
  | TokenPlus AlexPosn
  | TokenSub AlexPosn
  | TokenMult AlexPosn
  | TokenDiv AlexPosn
  | TokenBool AlexPosn !Bool
  | TokenLte AlexPosn
  | TokenGeq AlexPosn
  | TokenEq AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenFloat AlexPosn !Float
  | TokenNaN AlexPosn
  | TokenLid AlexPosn String

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
  show (TokenLte _)    = "<="
  show (TokenGeq _)    = ">="
  show (TokenEq _)     = "=="
  show (TokenIf _)     = "if"
  show (TokenThen _)   = "then"
  show (TokenElse _)   = "else"
  show (TokenFloat _ f) = show f
  show (TokenNaN _)    = "NaN"
  show (TokenLid _ s)  = "LID: " ++ s

tokenPosition :: Token -> Pos
tokenPosition (TokenLParen (AlexPn _ line col))   = (line,col)
tokenPosition (TokenRParen (AlexPn _ line col))   = (line,col)
tokenPosition (TokenInt (AlexPn _ line col) _ )   = (line,col)
tokenPosition (TokenPlus (AlexPn _ line col))     = (line,col)
tokenPosition (TokenSub (AlexPn _ line col))      = (line,col)
tokenPosition (TokenMult (AlexPn _ line col))     = (line,col)
tokenPosition (TokenDiv (AlexPn _ line col))      = (line,col)
tokenPosition (TokenBool (AlexPn _ line col) _ )  = (line,col)
tokenPosition (TokenLte (AlexPn _ line col))      = (line,col)
tokenPosition (TokenGeq (AlexPn _ line col))      = (line,col)
tokenPosition (TokenEq (AlexPn _ line col))       = (line,col)
tokenPosition (TokenIf (AlexPn _ line col))       = (line,col)
tokenPosition (TokenThen (AlexPn _ line col))     = (line,col)
tokenPosition (TokenElse (AlexPn _ line col))     = (line,col)
tokenPosition (TokenFloat (AlexPn _ line col) _ ) = (line,col)
tokenPosition (TokenNaN (AlexPn _ line col))      = (line,col)
tokenPosition (TokenLid (AlexPn _ line col) _)    = (line,col)
}
