{

module Parser where

import Lexer
import Lang
import Error
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
     '<='       { TokenLte _}
     '>='       { TokenGeq _}
     '=='       { TokenEq _}
     if         { TokenIf _}
     then       { TokenThen _}
     else       { TokenElse _}
     NaN        { TokenNaN _}
     int        { TokenInt _ _}
     bool       { TokenBool _ _}
     float      { TokenFloat _ _}

%nonassoc '<=' '>=' '=='
%left '+' '-'
%left '*' '/'

%%

exp :: { Exp Pos }
exp : if exp then exp else exp  { PosExp (tokenPosition $1) (EIf $2 $4 $6) }
    | exp1                      { $1 }

exp1 :: { Exp Pos}
exp1 : int                      { extractTokenContents $1 }
     | float                    { extractTokenContents $1 }
     | bool                     { extractTokenContents $1 }
     | NaN                      { PosExp (tokenPosition $1) ENaN}
     | '(' exp ')'              { $2 }
     | exp1 '+' exp1            { PosExp (tokenPosition $2) (EOp Plus $1 $3) }
     | exp1 '-' exp1            { PosExp (tokenPosition $2) (EOp Minus $1 $3) }
     | exp1 '*' exp1            { PosExp (tokenPosition $2) (EOp Mult $1 $3) }
     | exp1 '/' exp1            { PosExp (tokenPosition $2) (EOp Div $1 $3) }
     | exp1 '<=' exp1           { PosExp (tokenPosition $2) (EOp Lte $1 $3) }
     | exp1 '>=' exp1           { PosExp (tokenPosition $2) (EOp Geq $1 $3) }
     | exp1 '==' exp1           { PosExp (tokenPosition $2) (EOp Eq $1 $3) }

{
extractTokenContents :: Token -> Exp Pos
extractTokenContents (TokenInt (AlexPn _ line col) n)   = PosExp (line,col) (EInt n)
extractTokenContents (TokenFloat (AlexPn _ line col) f) = PosExp (line,col) (EFloat f)
extractTokenContents (TokenBool (AlexPn _ line col) b)  = PosExp (line,col) (EBool b)
extractTokenContents _ = error "This should never happen. Weird..."

parseError :: [Token] -> a
parseError (t:ts) = posError (tokenPosition t) "Parse Error" ""
parseError [] = errorWithoutStackTrace "Parse Error: Reached EOF without closing expression"
}

