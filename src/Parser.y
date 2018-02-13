{

module Parser where

import Lexer
import Lang
}

%name parse
%tokentype { Token }
%error { parseError }

%token
     '('        { TokenLParen _}
     ')'        { TokenRParen _}
     '+'        { TokenPlus _}
     '-'        { TokenSub _}
     '*'        { TokenMult _}
     '/'        { TokenDiv _}
     '<='       { TokenLEQ _}
     if         { TokenIf _}
     NaN        { TokenNaN _}
     int        { TokenInt _ _}
     bool       { TokenBool _ _}
     float      { TokenFloat _ _}

%%

exp :: { Exp Pos}
exp : int                 { extractTokenContents $1 }
    | float               { extractTokenContents $1 }
    | bool                { extractTokenContents $1 }
    | NaN                 { PosExp (tokenPosition $1) ENaN}
    | '(' '+' exp exp ')' { PosExp (tokenPosition $1) (EOp Plus $3 $4) }
    | '(' '-' exp exp ')' { PosExp (tokenPosition $1) (EOp Minus $3 $4) }
    | '(' '*' exp exp ')' { PosExp (tokenPosition $1) (EOp Mult $3 $4) }
    | '(' '/' exp exp ')' { PosExp (tokenPosition $1) (EOp Div $3 $4) }
    | '(' '<=' exp exp ')' { PosExp (tokenPosition $1) (ELeq $3 $4) }
    | '(' if exp exp exp ')' { PosExp (tokenPosition $1) (EIf $3 $4 $5) }
{
extractTokenContents :: Token -> Exp Pos
extractTokenContents (TokenInt (AlexPn _ line col) n)   = PosExp (line,col) (EInt n)
extractTokenContents (TokenFloat (AlexPn _ line col) f) = PosExp (line,col) (EFloat f)
extractTokenContents (TokenBool (AlexPn _ line col) b)  = PosExp (line,col) (EBool b)
extractTokenContents _ = error "This should never happen. Weird..."


parseError :: [Token] -> a
parseError (t:ts) = errorWithoutStackTrace ("Parse Error at " ++ show (tokenPosition t))
parseError [] = errorWithoutStackTrace "Parse Error: Reached EOF without closing expression"
}

