{-# LANGUAGE DeriveGeneric #-}
module Lang where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map

type Prog = ([Decl], Exp Pos)

type Id = String
type Pos = (Int,Int)

type Context = Map.Map Id Typ
type Env = (Int, Map.Map Int (Exp Pos))

type Ctor = (Id, Typ)

data Decl = DData Id [Ctor]

data Typ = TInt
  | TBool
  | TFloat
  | TArr !Typ !Typ
  | TUnit
  | TPair !Typ !Typ
  | TList !Typ
  | TRef !Typ
  | TData !Id
  | TUnknown
  deriving(Generic)

instance NFData Typ

-- NOTE: Reversed
arrToList :: Typ -> [Typ]
arrToList (TArr t1 t2) = t2 : arrToList t1
arrToList t = [t]

data Value = VInt {-# UNPACK #-} !Int
  | VBool !Bool
  | VFloat {-# UNPACK #-} !Float
  | VFun !Id !(Exp Pos)
  | VRec !Id !Id !(Exp Pos)
  | VNaN
  | VUnit
  | VPair !Value !Value
  | VList !(Exp Pos)
  | VPtr !Int
  | VData !Id ![Value]
  deriving (Generic, Eq)

-- Credit: Andrew Mack for helping me scraping my boilerplate
data Op = Plus
  | Minus
  | Mult
  | Div
  | Lte
  | Geq
  | Eq
  | Lt
  | Gt
  | Mod
  deriving (Generic, Eq)

data Exp t = PosExp t Typ (Exp_ t)
  deriving (Eq)

data Exp_ t = EInt !Int
  | EFloat !Float
  | EBool !Bool
  | EVar !Id
  | EFun !(Exp t) !(Exp t)
  | ERec !(Exp t) !(Exp t) !(Exp t)
  | ENaN
  | EOp !Op !(Exp t) !(Exp t)
  | EIf !(Exp t) (Exp t) (Exp t)
  | ELet !(Exp t) !(Exp t) !(Exp t)
  | EFunApp !(Exp t) !(Exp t)
  | EUnit
  | EPair !(Exp t) !(Exp t)
  | EFst !(Exp t)
  | ESnd !(Exp t)
  | ENil
  | ECons !(Exp t) !(Exp t)
  | EHead !(Exp t)
  | ETail !(Exp t)
  | EEmpty !(Exp t)
  | ERef !(Exp t)
  | EPtr !Int
  | ESet !(Exp t) !(Exp t)
  | EBang !(Exp t)
  | ESeq !(Exp t) !(Exp t)
  | EWhile !(Exp t) !(Exp t)
  | EMatch !(Exp t) ![Branch t]
  | ECtor !Id ![Exp t]
  deriving (Generic, Eq)

type Branch t = (Pattern, Exp t)

data Pattern = PWildCard
  | PVar !Id
  | PPair !(Pattern, Pattern)
  | PList !(Pattern, Pattern)
  | PCtor !Id ![Pattern]
  deriving (Generic, Eq, Show)

instance Show Typ where
  show TInt = "Int"
  show TFloat = "Float"
  show TBool = "Bool"
  show (TArr t1 t2) = "(TArr " ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show TUnit = "Unit"
  show (TPair t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TList t) = "[" ++ show t ++ "]"
  show (TRef t) = "<" ++ show t ++ ">"
  show (TData s) = s
  show TUnknown = "TUnknown"

instance Show Value where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VFun s v)    = "lambda " ++ s ++ " -> " ++ show v
  show (VRec f l v)  = "fix " ++ f ++ " " ++ l ++ " -> " ++ show v
  show (VFloat f)    = show f
  show VNaN          = "NaN"
  show VUnit         = "()"
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show (VList v)     = "[" ++ show v ++ "]"
  show (VPtr v)      = "Ptr(" ++ show v ++ ")"
  show (VData x vs)  = let showValues :: [Value] -> String
                           showValues (v:vs') = show v ++ " " ++ showValues vs'
                           showValues [] = ""
                       in x ++ " " ++ showValues vs

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show Lte   = "<="
  show Geq   = ">="
  show Eq    = "=="
  show Lt    = "<"
  show Gt    = ">"
  show Mod   = "%"

instance Show (Exp t) where
  show (PosExp _ _ e) = show e

instance Show (Exp_ t) where
  show (EInt n) = show n
  show (EOp op e1 e2) = "(" ++ show e1 ++ " " ++  show op ++ " " ++ show e2 ++ ")"
  show (EBool True) = "true"
  show (EBool False) = "false"
  show (ELet l e1 e2) = "let " ++ show l ++ " = " ++ show e1 ++ " in " ++ show e2
  show (EFun l e) = "lambda " ++ show l ++ " -> " ++ show e
  show (ERec f l v) = "fix " ++ show f ++ " " ++ show l ++ " -> " ++ show v
  show (EFunApp e1 e2) = "(lambda " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then  " ++ show e2 ++ " else " ++ show e3
  show (EFloat f) = show f
  show (EVar s)   = s
  show ENaN = "NaN"
  show EUnit = "()"
  show (EPair e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (EFst e) = "fst " ++ show e
  show (ESnd e) = "snd " ++ show e
  show ENil = "[]"
  show (ECons e1 e2) = case e2 of
    (PosExp _ _ ENil) -> show e1
    _ -> show e1 ++ ", " ++ show e2
  show (EHead e) = "head " ++ show e
  show (ETail e) = "tail " ++ show e
  show (EEmpty e) = "empty " ++ show e
  show (ERef e) = "ref " ++ show e
  show (EPtr e) = "ptr(" ++ show e ++ ")"
  show (ESet e1 e2) = show e1 ++ " := " ++ show e2
  show (EBang e) = "!" ++ show e
  show (ESeq e1 e2) = show e1 ++ " ; " ++ show e2
  show (EWhile e1 e2) = "while " ++ show e1 ++ " do " ++ show e2 ++ " end"
  show (EMatch e ps) = "case " ++ show e ++ " of " ++ show ps ++ " end"
  show (ECtor id' bs) = id' ++ (if not $ null bs then show bs else "")

instance Eq Typ where
  TInt == TInt = True
  TFloat == TFloat = True
  TBool == TBool = True
  (TArr t1 t2) == (TArr t1' t2') = (t1 == t1' && t2 == t2') || (t1 == t2' && t2 == t1')
  TUnit == TUnit = True
  (TPair t1 t2) == (TPair t1' t2') = t1 == t1' && t2 == t2'
  (TList t1) == (TList t2) = t1 == t2
  (TRef t1) == (TRef t2) = t1 == t2
  (TData s1) == (TData s2) = s1 == s2
  TUnknown == TUnknown = True
  _ == _ = False
