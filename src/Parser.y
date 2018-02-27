{

module Parser where

import Lexer
import Lang
import Error
}

%name parse
%tokentype { Token }
%error { parseError }

%expect 40
%nonassoc '<=' '>=' '==' '<' '>'
%left '+' '-'
%left '*' '/' '%'

%token
     '('        { TokenLParen _}
     ')'        { TokenRParen _}
     '+'        { TokenPlus _}
     '-'        { TokenSub _}
     '*'        { TokenMult _}
     '/'        { TokenDiv _}
     '='        { TokenSet _}
     '<'        { TokenLess _}
     '>'        { TokenGreat _}
     '%'        { TokenMod _}
     ':'        { TokenColon _}
     '<='       { TokenLte _}
     '>='       { TokenGeq _}
     '=='       { TokenEq _}
     '->'       { TokenArr _}
     '=>'       { TokenFat _}
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
     uid        { TokenUid _ _}
%%

exp :: { Exp Pos }
exp : exp '+' exp            { PosExp (tokenPosition $2) TUnit (EOp Plus $1 $3) }
    | exp '-' exp            { PosExp (tokenPosition $2) TUnit (EOp Minus $1 $3) }
    | exp '*' exp            { PosExp (tokenPosition $2) TUnit (EOp Mult $1 $3) }
    | exp '/' exp            { PosExp (tokenPosition $2) TUnit (EOp Div $1 $3) }
    | exp '<=' exp           { PosExp (tokenPosition $2) TUnit (EOp Lte $1 $3) }
    | exp '>=' exp           { PosExp (tokenPosition $2) TUnit (EOp Geq $1 $3) }
    | exp '==' exp           { PosExp (tokenPosition $2) TUnit (EOp Eq $1 $3) }
    | exp '<' exp            { PosExp (tokenPosition $2) TUnit (EOp Lt $1 $3) }
    | exp '>' exp            { PosExp (tokenPosition $2) TUnit (EOp Gt $1 $3) }
    | exp '%' exp            { PosExp (tokenPosition $2) TUnit (EOp Mod $1 $3) } 
    | lexp                     { $1 }

lexp : if exp then exp else exp              { PosExp (tokenPosition $1) TUnit (EIf $2 $4 $6) }
     | let lid ':' ':' typ '=' exp in exp    { PosExp (tokenPosition $1) TUnit (ELet (extractVar $2 $5) $7 $9) }
     | fun                                   { $1 }
     | fapp                                  { $1 }

fapp :: { Exp Pos }
fapp : fapp val                { PosExp (extractExpPos $1) TUnit (EFunApp $1 $2) }
     | val                     { $1 }

val :: { Exp Pos }
val : int                       { extractTokenContents $1 }
    | float                     { extractTokenContents $1 }
    | bool                      { extractTokenContents $1 }
    | lid                       { extractVar $1 TUnit }
    | NaN                       { PosExp (tokenPosition $1) TFloat ENaN}
    | '(' ')'                   { PosExp (tokenPosition $1) TUnit EUnit}
    | '(' exp ')'               { $2 }

typ :: { Typ }
typ : uid '->' typ              { TArr (extractTyp $1) $3 }
    | uid                       { extractTyp $1 }
    | '(' typ ')'               { $2 }

fun :: { Exp Pos }
fun : lambda '(' lid ':' ':' typ ')' ':' ':' typ '=>' exp       { PosExp
(tokenPosition $1) (TArr $6 $10) (EFun (extractVar $3 $6) $12) }
    | fix lid '(' lid ':' ':' typ ')' ':' ':' typ '=>' exp      { PosExp (tokenPosition $1) (TArr $7 $11) (ERec (extractVar $2 (TArr $7 $11)) (extractVar $4 $7) $13) }
{
extractTokenContents :: Token -> Exp Pos
extractTokenContents (TokenInt (AlexPn _ line col) n)   = PosExp (line,col) TInt (EInt n)
extractTokenContents (TokenFloat (AlexPn _ line col) f) = PosExp (line,col) TFloat (EFloat f)
extractTokenContents (TokenBool (AlexPn _ line col) b)  = PosExp (line,col) TBool (EBool b)
extractTokenContents _ = error "This should never happen. Weird..."

extractVar :: Token -> Typ -> Exp Pos
extractVar (TokenLid (AlexPn _ line col) s) t = PosExp (line,col) t (EVar s)

extractTyp :: Token -> Typ
extractTyp (TokenUid _ "Int") = TInt
extractTyp (TokenUid _ "Bool") = TBool
extractTyp (TokenUid _ "Float") = TFloat
extractTyp (TokenUid (AlexPn _ line col) s) = posError (line,col) "Parse Error"(": undefined type " ++ show s)

extractExpPos :: Exp Pos -> Pos
extractExpPos (PosExp p _ _) = p

parseError :: [Token] -> a
parseError (t:ts) = posError (tokenPosition t) "Parse Error" (show ts)
parseError [] = errorWithoutStackTrace "Parse Error: Reached EOF without closing expression"
}
