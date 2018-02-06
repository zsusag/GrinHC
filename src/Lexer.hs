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
lexStringH ('<':'=':xs) ts = lexStringH xs (TokenLEQ:ts)
lexStringH ('i':'f':xs) ts = lexStringH xs (TokenIf:ts)
lexStringH ('t':'r':'u':'e':xs) ts = lexStringH xs (TokenBool True:ts)
lexStringH ('f':'a':'l':'s':'e':xs) ts = lexStringH xs (TokenBool False:ts)
lexStringH ('N':'a':'N':xs) ts = lexStringH xs (TokenNaN:ts)
lexStringH (x:xs) ts
  | isDigit x =
    let tokenInt = reverse $ lexInt xs [x]
        xs'      = drop (length tokenInt - 1) xs
    in if head xs' == '.'
       then let decimal = reverse $ lexInt (drop 1 xs') []
                xs''    = drop (length decimal + 1) xs'
            in lexStringH xs'' (TokenFloat (read (tokenInt ++ "." ++ decimal)):ts)
       else lexStringH xs' (TokenInt (read tokenInt):ts)
  | isSpace x = lexStringH xs ts
  | otherwise =  error ("Invalid character: " ++ [x])
lexStringH [] ts = ts

lexInt :: String -> String -> String
lexInt (x:xs) digits = if isDigit x
                       then lexInt xs (x:digits)
                       else digits
lexInt [] digits = digits
