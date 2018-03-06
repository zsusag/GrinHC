{

module Parser where

import Lexer
import Lang
import Error
}

%name parse
%tokentype { Token }
%error { parseError }

%expect 52
%nonassoc '<=' '>=' '==' '<' '>'
%right ';'
%left ':='
%left '+' '-'
%left '*' '/' '%'
%right ':' 
%left fst snd head tail empty ref '!'
%right '->'

%token
     '('        { TokenLParen _}
     ')'        { TokenRParen _}
     '['        { TokenLBracket _}
     ']'        { TokenRBracket _}
     '+'        { TokenPlus _}
     '-'        { TokenSub _}
     '*'        { TokenMult _}
     '/'        { TokenDiv _}
     '='        { TokenSet _}
     '<'        { TokenLess _}
     '>'        { TokenGreat _}
     '%'        { TokenMod _}
     ':'        { TokenColon _}
     ';'        { TokenSemi _}
     ','        { TokenComma _}
     '!'        { TokenBang _}
     '<='       { TokenLte _}
     '>='       { TokenGeq _}
     '=='       { TokenEq _}
     '->'       { TokenArr _}
     '=>'       { TokenFat _}
     ':='       { TokenAssign _}
     ref        { TokenRef _}
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
     fst        { TokenFst _}
     snd        { TokenSnd _}
     head       { TokenHead _}
     tail       { TokenTail _}
     empty      { TokenEmpty _}
     while      { TokenWhile _}
     do         { TokenDo _}
     end        { TokenEnd _}
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
    | exp ':' exp            { PosExp (tokenPosition $2) TUnit (ECons $1 $3) }
    | exp ';' exp            { PosExp (tokenPosition $2) TUnit (ESeq $1 $3) }
    | exp ':=' exp           { PosExp (tokenPosition $2) TUnit (ESet $1 $3) }
    | fst exp                { PosExp (tokenPosition $1) TUnit (EFst $2) }
    | snd exp                { PosExp (tokenPosition $1) TUnit (ESnd $2) }
    | head exp               { PosExp (tokenPosition $1) TUnit (EHead $2) }
    | tail exp               { PosExp (tokenPosition $1) TUnit (ETail $2) }
    | empty exp              { PosExp (tokenPosition $1) TUnit (EEmpty $2) }
    | '!' exp                { PosExp (tokenPosition $1) TUnit (EBang $2) }
    | ref exp                { PosExp (tokenPosition $1) TUnit (ERef $2) }
    | lexp                   { $1 }

lexp : if exp then exp else exp              { PosExp (tokenPosition $1) TUnit (EIf $2 $4 $6) }
     | let lid ':' ':' typ '=' exp in exp    { PosExp (tokenPosition $1) TUnit (ELet (extractVar $2 $5) $7 $9) }
     | while exp do exp end                  { PosExp (tokenPosition $1) TUnit (EWhile $2 $4) }
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
    | '[' ']' ':' ':' typ       { PosExp (tokenPosition $1) $5 ENil }
    | '(' ')'                   { PosExp (tokenPosition $1) TUnit EUnit}
    | '(' exp ',' exp ')'       { PosExp (tokenPosition $1) TUnit (EPair $2 $4) }
    | '(' exp ')'               { $2 }

typ :: { Typ }
typ : typ '->' typ              { TArr $1 $3 }
    | uid                       { extractTyp $1 }
    | '[' typ ']'               { TList $2 }
    | '(' typ ',' typ ')'       { TPair $2 $4 }
    | '(' typ ')'               { $2 }
    | '<' typ '>'               { TRef $2 }
    | '(' ')'                   { TUnit }

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
