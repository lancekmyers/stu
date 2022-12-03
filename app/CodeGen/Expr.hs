{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module CodeGen.Expr where

import AST
    ( BinOp(Div, Add, Mul, Sub),
      Expr,
      ExprF(..),
      VarDomain(Param, Unknown, Local, Bound, Val, Data) )
import Control.Comonad.Trans.Cofree (tailF)
import Data.Functor.Foldable ( Recursive(cata) )
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import CodeGen.Python
    ( (@@), jnp, PyExp(..) ) 
import qualified Data.Text as T

cgExpr :: Expr w -> PyExp
cgExpr = cata (go . proj)
  where 
    proj = tailF . runIdentity . getCompose
    go :: ExprF PyExp -> PyExp
    go (ArithF op x y) = case op of
      Add -> jnp "add" @@ [x, y]
      Mul -> jnp "multiply" @@ [x, y]
      Sub -> jnp "subtract" @@ [x, y]
      Div -> jnp "divide" @@ [x, y]
    go (LitInt n) = PyNum (Right n)
    go (LitReal x) = PyNum (Left x) 
    go (LitArray xs) = PyList xs
    go (VarF name Unknown) = PyIdent [] (name <> "_ack")
    go (VarF name Local) = PyIdent [] ("_local_" <> name)
    go (VarF name Bound) = PyIdent [] ("_bd_" <> name)
    go (VarF name Val)   = PyIdent [] (name <> "_val")
    go (VarF name Data)  = PyGet (PyIdent [] "data") (PyStr name)
    go (VarF name Param) = PyIdent [] (name <> "_tr")
    go (FunAppF "mean" xs) = PyApply "jnp.mean" xs 
      [("axis", PyNum $ Right (-1))]
    go (FunAppF f xs) = jnp f @@ xs
    go (GatherF xs is) = PyGet xs is -- jnp "gather" @@ [xs, is]

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