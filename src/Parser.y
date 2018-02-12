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

exp :: { Exp }
exp : int                 { extractTokenContents $1 }
    | float               { extractTokenContents $1 }
    | bool                { extractTokenContents $1 }
    | NaN                 { ENaN (tokenPosition $1)}
    | '(' '+' exp exp ')' { EAdd $3 $4 (tokenPosition $1) }
    | '(' '-' exp exp ')' { ESub $3 $4 (tokenPosition $1) }
    | '(' '*' exp exp ')' { EMul $3 $4 (tokenPosition $1) }
    | '(' '/' exp exp ')' { EDiv $3 $4 (tokenPosition $1) }
    | '(' '<=' exp exp ')' { ELeq $3 $4 (tokenPosition $1) }
    | '(' if exp exp exp ')' { EIf $3 $4 $5 (tokenPosition $1)}
{
extractTokenContents :: Token -> Exp
extractTokenContents (TokenInt (AlexPn _ line col) n)   = EInt n (line,col)
extractTokenContents (TokenFloat (AlexPn _ line col) f) = EFloat f (line,col)
extractTokenContents (TokenBool (AlexPn _ line col) b)  = EBool b (line,col)
extractTokenContents _ = error "This should never happen. Weird..."


parseError :: [Token] -> a
parseError (t:ts) = let (line,col) = tokenPosition t
                    in error ("Parse Error at line " ++ show line ++ ", column "
                              ++ show col)
parseError [] = errorWithoutStackTrace "Parse Error: Reached EOF without closing expression"
}

