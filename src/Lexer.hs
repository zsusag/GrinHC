module Lexer where

import Data.Char

import Token

lexString :: String -> IO [Token]
lexString s = return $ reverse $ lexStringH s []

lexStringH :: String -> [Token] -> [Token]
lexStringH ('(':xs) ts = lexStringH xs (TokenLParen:ts)
lexStringH (')':xs) ts = lexStringH xs (TokenRParen:ts)
lexStringH ('+':xs) ts = lexStringH xs (TokenPlus:ts)
lexStringH ('-':xs) ts = lexStringH xs (TokenSub:ts)
lexStringH ('*':xs) ts = lexStringH xs (TokenMult:ts)
lexStringH ('/':xs) ts = lexStringH xs (TokenDiv:ts)
lexStringH (x:xs) ts
  | isDigit x =
    let tokenInt = reverse $ lexInt xs [x] in
      lexStringH (drop (length tokenInt - 1) xs)
      (TokenInt (read tokenInt):ts)
  | isSpace x = lexStringH xs ts
  | otherwise =  error ("Invalid character: " ++ [x])
lexStringH [] ts = ts

lexInt :: String -> String -> String
lexInt (x:xs) digits = if isDigit x
                       then lexInt xs (x:digits)
                       else digits
lexInt [] digits = digits
