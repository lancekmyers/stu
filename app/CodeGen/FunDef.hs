module CodeGen.FunDef where

import AST (FunBody (..), FunDef (FunDef), PrimApp (..), 
  Elaboration)
import CodeGen.Expr (cgExpr)
import CodeGen.Python
  ( PyCode (..),
    PyExp (PyIdent),
    (@@),
  )
import Types (Ty)

cgFunDef :: FunDef Elaboration -> PyCode
cgFunDef (FunDef name args ret body) = PyDef Nothing name' argNames body'
  where
    argNames = ("_bd_" <>) . fst <$> args
    name' = "_func_" <> name
    body' = cgFunBody body

cgFunBody :: FunBody Elaboration -> PyCode
cgFunBody (FunRet val) = PyRet $ cgExpr val
cgFunBody (LetPrimIn x _ pApp rest) =
  PyBlock [PyAssign ("_local_" <> x) (cgPrimApp pApp), cgFunBody rest]
cgFunBody (FunLetIn x _ val rest) =
  PyBlock [PyAssign ("_local_" <> x) (cgExpr val), cgFunBody rest]

cgPrimApp :: PrimApp Elaboration -> PyExp
cgPrimApp (PrimApp name args) = (PyIdent mod f) @@ (cgExpr <$> args)
  where
    mod = init name
    f = last name
