{

module Parser where

import Lexer
import Lang
import Error
}

%name parse
%tokentype { Token }
%error { parseError }

%nonassoc '<=' '>=' '==' '<' '>' then else
%right in
%left '+' '-'
%left '*' '/'
%left '$'
%left '->'

%token
     '('        { TokenLParen _}
     ')'        { TokenRParen _}
     '+'        { TokenPlus _}
     '-'        { TokenSub _}
     '*'        { TokenMult _}
     '/'        { TokenDiv _}
     '='        { TokenSet _}
     '$'        { TokenDollar _}
     '<'        { TokenLess _}
     '>'        { TokenGreat _}
     '<='       { TokenLte _}
     '>='       { TokenGeq _}
     '=='       { TokenEq _}
     '->'       { TokenArr _}
     let        { TokenLet _}
     in         { TokenIn _}
     lambda     { TokenLambda _}
     fix        { TokenFix _}
     if         { TokenIf _}
     then       { TokenThen _}
     else       { TokenElse _}
     NaN        { TokenNaN _}
     int        { TokenInt _ _}
     bool       { TokenBool _ _}
     float      { TokenFloat _ _}
     lid        { TokenLid _ _}

%%

exp :: { Exp Pos }
exp : if exp then exp else exp  { PosExp (tokenPosition $1) (EIf $2 $4 $6) }
    | let lid '=' exp in exp    { PosExp (tokenPosition $1) (ELet (extractTokenContents $2) $4 $6) }
    | exp '$' exp               { PosExp (extractExpPos $1) (EFunApp $1 $3) }
    | exp1                      { $1 }

exp1 :: { Exp Pos}
exp1 : exp1 '+' exp1             { PosExp (tokenPosition $2) (EOp Plus $1 $3) }
     | exp1 '-' exp1             { PosExp (tokenPosition $2) (EOp Minus $1 $3) }
     | exp1 '*' exp1             { PosExp (tokenPosition $2) (EOp Mult $1 $3) }
     | exp1 '/' exp1             { PosExp (tokenPosition $2) (EOp Div $1 $3) }
     | exp1 '<=' exp1            { PosExp (tokenPosition $2) (EOp Lte $1 $3) }
     | exp1 '>=' exp1            { PosExp (tokenPosition $2) (EOp Geq $1 $3) }
     | exp1 '==' exp1            { PosExp (tokenPosition $2) (EOp Eq $1 $3) }
     | exp1 '<' exp1             { PosExp (tokenPosition $2) (EOp Lt $1 $3) }
     | exp1 '>' exp1             { PosExp (tokenPosition $2) (EOp Gt $1 $3) }
     | '(' exp ')'              { $2 }
     | val                      { $1 }

val :: { Exp Pos }
val : int                       { extractTokenContents $1 }
    | float                     { extractTokenContents $1 }
    | bool                      { extractTokenContents $1 }
    | lid                       { extractTokenContents $1 }
    | fun                       { $1 }
    | NaN                       { PosExp (tokenPosition $1) ENaN}

fun :: { Exp Pos }
fun : lambda lid '->' exp       { PosExp (tokenPosition $1) (EFun (extractTokenContents $2) $4) }
--    | fix lid lid '->' exp      { PosExp (tokenPosition $1) (ERec (extractTokenContents $2) (extractTokenContents $3) $5) }
{
extractTokenContents :: Token -> Exp Pos
extractTokenContents (TokenInt (AlexPn _ line col) n)   = PosExp (line,col) (EInt n)
extractTokenContents (TokenFloat (AlexPn _ line col) f) = PosExp (line,col) (EFloat f)
extractTokenContents (TokenBool (AlexPn _ line col) b)  = PosExp (line,col) (EBool b)
extractTokenContents (TokenLid (AlexPn _ line col) s)   = PosExp (line,col) (ELid s)
extractTokenContents _ = error "This should never happen. Weird..."

extractExpPos :: Exp Pos -> Pos
extractExpPos (PosExp p _) = p

parseError :: [Token] -> a
parseError (t:ts) = posError (tokenPosition t) "Parse Error" ""
parseError [] = errorWithoutStackTrace "Parse Error: Reached EOF without closing expression"
}

