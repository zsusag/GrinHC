{

module Parser where

import Lexer
import Lang
import Error
}

%name parse
%tokentype { Token }
%error { parseError }

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
exp : if exp then exp else exp              { PosExp (tokenPosition $1) TUnknown (EIf $2 $4 $6) }
    | let lid ':' ':' typ '=' exp in exp    { PosExp (tokenPosition $1) $5 (ELet (extractTokenContents $2) $7 $9) }
    | fun                                   { $1 }
    | fapp                                  { $1 }

fapp :: { Exp Pos }
fapp : fapp exp1                { PosExp (extractExpPos $1) TUnknown (EFunApp $1 $2) }
     | exp1                     { $1 }

exp1 :: { Exp Pos}
exp1 : exp1 '+' exp1            { PosExp (tokenPosition $2) TUnknown (EOp Plus $1 $3) }
     | exp1 '-' exp1            { PosExp (tokenPosition $2) TUnknown (EOp Minus $1 $3) }
     | exp1 '*' exp1            { PosExp (tokenPosition $2) TUnknown (EOp Mult $1 $3) }
     | exp1 '/' exp1            { PosExp (tokenPosition $2) TUnknown (EOp Div $1 $3) }
     | exp1 '<=' exp1           { PosExp (tokenPosition $2) TUnknown (EOp Lte $1 $3) }
     | exp1 '>=' exp1           { PosExp (tokenPosition $2) TUnknown (EOp Geq $1 $3) }
     | exp1 '==' exp1           { PosExp (tokenPosition $2) TUnknown (EOp Eq $1 $3) }
     | exp1 '<' exp1            { PosExp (tokenPosition $2) TUnknown (EOp Lt $1 $3) }
     | exp1 '>' exp1            { PosExp (tokenPosition $2) TUnknown (EOp Gt $1 $3) }
     | exp1 '%' exp1            { PosExp (tokenPosition $2) TUnknown (EOp Mod $1 $3) }
     | '(' exp ')'              { $2 }
     | val                      { $1 }

val :: { Exp Pos }
val : int                       { extractTokenContents $1 }
    | float                     { extractTokenContents $1 }
    | bool                      { extractTokenContents $1 }
    | lid                       { extractTokenContents $1 }
    | NaN                       { PosExp (tokenPosition $1) TFloat ENaN}

typ :: { Typ }
typ : typ '->' uid              { TArr $1 (extractTyp $3) }
    | uid                       { extractTyp $1 }

fun :: { Exp Pos }
fun : lambda '(' lid ':' ':' typ ')' ':' ':' typ '->' exp       { PosExp (tokenPosition $1) (TArr $6 $10) (EFun (extractTokenContents $3) $12) }
    | fix lid '(' lid ':' ':' typ ')' ':' ':' typ '->' exp      { PosExp (tokenPosition $1) (TArr $7 $11) (ERec (extractTokenContents $2) (extractTokenContents $4) $13) }
{
extractTokenContents :: Token -> Exp Pos
extractTokenContents (TokenInt (AlexPn _ line col) n)   = PosExp (line,col) TUnknown (EInt n)
extractTokenContents (TokenFloat (AlexPn _ line col) f) = PosExp (line,col) TUnknown (EFloat f)
extractTokenContents (TokenBool (AlexPn _ line col) b)  = PosExp (line,col) TUnknown (EBool b)
extractTokenContents (TokenLid (AlexPn _ line col) s)   = PosExp (line,col) TUnknown (ELid s)
extractTokenContents _ = error "This should never happen. Weird..."

extractTyp :: Token -> Typ
extractTyp (TokenUid _ "Int") = TInt
extractTyp (TokenUid _ "Bool") = TBool
extractTyp (TokenUid _ "Float") = TFloat

extractExpPos :: Exp Pos -> Pos
extractExpPos (PosExp p _ _) = p

parseError :: [Token] -> a
parseError (t:ts) = posError (tokenPosition t) "Parse Error" ""
parseError [] = errorWithoutStackTrace "Parse Error: Reached EOF without closing expression"
}
