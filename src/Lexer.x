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
  \=                { tok (\p s -> TokenSet p) }
  \$                { tok (\p s -> TokenDollar p) }
  \<                { tok (\p s -> TokenLess p) }
  \>                { tok (\p s -> TokenGreat p) }
  "<="              { tok (\p s -> TokenLte p) }
  ">="              { tok (\p s -> TokenGeq p) }
  "=="              { tok (\p s -> TokenEq p) }
  "->"              { tok (\p s -> TokenArr p) }
  let               { tok (\p s -> TokenLet p) }
  in                { tok (\p s -> TokenIn p) }
  lambda            { tok (\p s -> TokenLambda p) }
  fix               { tok (\p s -> TokenFix p) }
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
  | TokenSet AlexPosn
  | TokenDollar AlexPosn
  | TokenLess AlexPosn
  | TokenGreat AlexPosn
  | TokenBool AlexPosn !Bool
  | TokenLte AlexPosn
  | TokenGeq AlexPosn
  | TokenEq AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenFloat AlexPosn !Float
  | TokenNaN AlexPosn
  | TokenLid AlexPosn !String
  | TokenArr AlexPosn
  | TokenLet AlexPosn
  | TokenIn AlexPosn
  | TokenLambda AlexPosn
  | TokenFix AlexPosn

instance Show Token where
  show (TokenLParen _) = "("
  show (TokenRParen _) = ")"
  show (TokenInt _ n)  = show n
  show (TokenPlus _)   = "+"
  show (TokenSub _)    = "-"
  show (TokenMult _)   = "*"
  show (TokenDiv _)    = "/"
  show (TokenSet _)    = "="
  show (TokenDollar _) = "$"
  show (TokenLess _)   = "<"
  show (TokenGreat _)  = ">"
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
  show (TokenLid _ s)  = s
  show (TokenArr _)    = "->"
  show (TokenLet _)    = "let"
  show (TokenIn _)     = "in"
  show (TokenLambda _) = "lambda"
  show (TokenFix _)    = "fix"

tokenPosition :: Token -> Pos
tokenPosition (TokenLParen (AlexPn _ line col))   = (line,col)
tokenPosition (TokenRParen (AlexPn _ line col))   = (line,col)
tokenPosition (TokenInt (AlexPn _ line col) _ )   = (line,col)
tokenPosition (TokenPlus (AlexPn _ line col))     = (line,col)
tokenPosition (TokenSub (AlexPn _ line col))      = (line,col)
tokenPosition (TokenMult (AlexPn _ line col))     = (line,col)
tokenPosition (TokenDiv (AlexPn _ line col))      = (line,col)
tokenPosition (TokenSet (AlexPn _ line col))      = (line,col)
tokenPosition (TokenDollar (AlexPn _ line col))   = (line,col)
tokenPosition (TokenLess (AlexPn _ line col))     = (line,col)
tokenPosition (TokenGreat (AlexPn _ line col))    = (line,col)
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
tokenPosition (TokenArr (AlexPn _ line col))      = (line,col)
tokenPosition (TokenLet (AlexPn _ line col))      = (line,col)
tokenPosition (TokenIn (AlexPn _ line col))       = (line,col)
tokenPosition (TokenLambda (AlexPn _ line col))   = (line,col)
tokenPosition (TokenFix (AlexPn _ line col))      = (line,col)
}
