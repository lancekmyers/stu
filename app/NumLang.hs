module NumLang where

import Data.List (intercalate)
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Stride = Stride (Vector Int) deriving (Show, Eq)

newtype Shape = Shape (Vector Int) deriving (Show, Eq)

type IdxVar = String

type Var = String

data Idx
  = LitIdx Int
  | BdIdx IdxVar

instance Show Idx where
  show (LitIdx i) = show i
  show (BdIdx x) = x

data Expr
  = Call Var [Expr]
  | VarRef Var
  | Get Var Idx

instance Show Expr where
  show (Call f xs) = f ++ "(" ++ show xs ++ ")"
  show (VarRef x) = x
  show (Get x is) = x ++ (show is)

newtype LoopName = LoopName String

instance Show LoopName where
  show (LoopName l) = '@' : l

data Card = CardLit Int | CardFV String

instance Show Card where
  show (CardLit n) = show n
  show (CardFV x) = x

data Stmt
  = Assign Var Idx Expr
  | For LoopName IdxVar Card Stmt
  | Seq Stmt Stmt

instance Show Stmt where
  show (Assign x i val) =
    x ++ show i ++ " := " ++ show val
  show (For loop i n stmts) =
    (show loop) ++ " : for " ++ i ++ " to " ++ show n
      ++ "{\n"
      ++ show stmts
      ++ "\n}"
  show (Seq s1 s2) = show s1 ++ "\n" ++ show s2

matMul = For "I" i n $ For "J" j m $ For "K" k o $ Assign c_mat [i, j]
