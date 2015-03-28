module GCL where
import Data.SBV

-- | The Stmt data type for our language
data Stmt
    = Skip
    | Assume Expr
    | Assert Expr
    | Inv    Expr Expr Stmt
    | (:&)   Stmt Stmt --sequencing
    | Box    Stmt Stmt -- []
    | (:=)   [String] [Expr]
    | Var    [(String, Type)] Stmt
    | SetArray String (Integer, Integer)
    deriving Show

data Type
    = Int
    | Bool
    | Array
    deriving (Show, Eq)

-- | Expressions + Predicates
data Expr
    = Ref    String    -- for retrieving var
    | RefA   Integer String    -- for retrieving array var
    | A      (SArray Integer Integer)
    | I      SInteger
    | B      SBool
    | ArrayRef String (Integer, Integer, Expr) -- Modify array
    | (:+)   Expr Expr
    | (:-)   Expr Expr
    | (:*)   Expr Expr
    | (:/)   Expr Expr
    | (:||)  Expr Expr
    | (:&&)  Expr Expr
    | (:^)   Expr Expr
    | V      Expr Expr
    | (:=>)  Expr Expr
    | (:<=>) Expr Expr
    | (:<)   Expr Expr
    | (:<=)  Expr Expr
    | (:>)   Expr Expr
    | (:>=)  Expr Expr
    | (:==)  Expr Expr
    | Not    Expr
    | TRUE
    | FALSE
    | InvExpr Int Expr Expr Expr Stmt
    | Forall [(String, Type)] Expr
    | Replace [String] [Expr] Expr
    deriving Show

infixl 3 :=
infixl 4 :-
infixl 4 :/
infixl 2 :&
