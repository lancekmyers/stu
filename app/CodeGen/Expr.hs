{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeGen.Expr where

import AST
  ( BinOp (Add, Div, Mul, Sub),
    Expr,
    ExprF (..),
    VarDomain (Bound, Data, Local, Param, Unknown, Val),
  )
import CodeGen.Python
  ( PyExp (..),
    jnp,
    lax,
    (!),
    (!$),
    (@@),
  )
import Control.Comonad.Trans.Cofree (CofreeF ((:<)), tailF)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Foldable (Recursive (cata))
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as T
import Types (Card (..), Shape, Ty (..), shToList, shape)

cgExpr :: Expr Ty -> PyExp
cgExpr = cata (\(Compose (Identity (ty :< e))) -> go ty e)
  where
    proj = tailF . runIdentity . getCompose
    go :: Ty -> ExprF PyExp -> PyExp
    go _ (ArithF op x y) = case op of
      Add -> jnp "add" @@ [x, y]
      Mul -> jnp "multiply" @@ [x, y]
      Sub -> jnp "subtract" @@ [x, y]
      Div -> jnp "divide" @@ [x, y]
    go _ (LitInt n) = PyNum (Right n)
    go _ (LitReal x) = PyNum (Left x)
    go _ (LitArray xs) = PyList xs
    go _ (VarF name Unknown) = PyIdent [] (name <> "_ack")
    go _ (VarF name Local) = PyIdent [] ("_local_" <> name)
    go _ (VarF name Bound) = PyIdent [] ("_bd_" <> name)
    go _ (VarF name Val) = PyIdent [] (name <> "_val")
    go _ (VarF name Data) = PyGet (PyIdent [] "data") (PyStr name)
    go _ (VarF name Param) = PyIdent [] (name <> "_tr")
    go _ (FunAppF "mean" xs) =
      PyApply
        "jnp.mean"
        xs
        [("axis", PyNum $ Right (-1))]
    go _ (FunAppF f xs) = jnp f @@ xs
    go _ (GatherF xs is) = PyGet xs is -- jnp "gather" @@ [xs, is]
    go t (ScatterAddF xs is) =
      let z = jnp "zeros" @@ [cgShape . shape $ t]
       in ((z !$ "at") [is]) !$ "add" $ [xs]
    go _ (FoldF f x0 xs) = lax "reduce" @@ [xs, x0, PyIdent [] f]
    go _ (ScanF f x0 xs) =
      lax "scan"
        @@ [ PyLambda ["c", "a"] $
               PyTuple
                 [ (PyIdent [] f) @@ [PyIdent [] "c", PyIdent [] "a"],
                   PyIdent [] "c"
                 ],
             x0,
             xs
           ]
    go _ (TransposeF x perm) =
      lax "transpose"
        @@ [x, PyList (PyNum . Right <$> perm)]

cgShape :: Shape -> PyExp
cgShape sh = PyList $ cgCard <$> (shToList $ sh)

cgCard :: Card -> PyExp
cgCard (CardFV n) = "cards" ! PyStr n
cgCard (CardN n) = PyNum $ Right n
cgCard (CardBV n) = "cards_local" ! PyStr n

cgFun (FunAppF "mean" xs) = PyApply "jnp.mean" xs [("axis", PyNum $ Right (-1))]
cgFun (FunAppF f xs) = case f of
  "matmul" -> jnp "dot" @@ xs
  "matvec" -> jnp "dot" @@ xs
  "vecmat" -> jnp "dot" @@ xs
  "dot" -> jnp "dot" @@ xs
  "sqrt" -> jnp "sqrt" @@ xs
  "exp" -> jnp "exp" @@ xs
  "log" -> jnp "log" @@ xs
  "sin" -> jnp "sin" @@ xs
  "cos" -> jnp "cos" @@ xs
  "tan" -> jnp "tan" @@ xs
  "tanh" -> jnp "tanh" @@ xs
  "sinh" -> jnp "sinh" @@ xs
  "cosh" -> jnp "cosh" @@ xs
  "asin" -> jnp "asin" @@ xs
  "acos" -> jnp "acos" @@ xs
  "atan" -> jnp "atan" @@ xs
  _ -> error $ "Encoutered function " ++ (T.unpack f) ++ " during codegen"
