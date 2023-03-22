module Parser.Expr (pExpr) where

import AST
  ( BinOp (..),
    ExprF (..),
    ExprSrc,
    VarDomain (..),
  )
import Control.Comonad.Trans.Cofree (CofreeF ((:<)), cofree)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL),
    makeExprParser,
  )
import Parser.Util
  ( Parser,
    integer,
    lexeme,
    pIdent,
    parens,
    signedFloat,
    signedInteger,
    symbol,
  )
import Text.Megaparsec
  ( MonadParsec (try),
    between,
    choice,
    getSourcePos,
    sepBy,
  )
import Util (mkPos)

withLoc parser = do
  from <- getSourcePos
  x <- parser
  to <- getSourcePos
  return . cofree $ (mkPos from to) :< x

pVariable :: Parser ExprSrc
pVariable = withLoc $ VarF <$> (lexeme pIdent) <*> (pure Unknown)

pLitInt :: Parser ExprSrc
pLitInt = withLoc $ LitInt . fromInteger <$> signedInteger

pLitReal :: Parser ExprSrc
pLitReal = withLoc $ LitReal <$> signedFloat

pLitArray :: Parser ExprSrc
pLitArray =
  withLoc $
    between
      (symbol "[")
      (symbol "]")
      (LitArray <$> sepBy pExpr (symbol ","))

pLit :: Parser (ExprSrc)
pLit =
  choice
    [ pLitArray,
      try pLitReal,
      pLitInt
    ]

pTerm :: Parser ExprSrc
pTerm =
  choice
    [ parens pExpr,
      try pFold,
      try pScan,
      try pTranspose,
      try pApp,
      try pVariable,
      pLit
    ]

pExpr :: Parser ExprSrc
pExpr = makeExprParser pTerm operatorTable

pAdd = do
  loc <- getSourcePos
  symbol "+"
  loc' <- getSourcePos
  return $ \x y -> cofree $ (mkPos loc loc') :< ArithF Add x y

pMul = do
  loc <- getSourcePos
  symbol "*"
  loc' <- getSourcePos
  return $ \x y -> cofree $ (mkPos loc loc') :< ArithF Mul x y

pDiv = do
  loc <- getSourcePos
  symbol "/"
  loc' <- getSourcePos
  return $ \x y -> cofree $ (mkPos loc loc') :< ArithF Div x y

pSub = do
  loc <- getSourcePos
  star <- symbol "-"
  loc' <- getSourcePos
  return $ \x y -> cofree $ (mkPos loc loc') :< ArithF Sub x y

operatorTable :: [[Operator Parser ExprSrc]]
operatorTable =
  [ [ InfixL pMul,
      InfixL pDiv
    ],
    [ InfixL pAdd,
      InfixL pSub
    ]
  ]

pFold = do
  let comma = symbol ","
  from <- getSourcePos
  "fold"
  fold <- parens $ FoldF <$> (pIdent <* comma) <*> (pExpr <* comma) <*> pExpr
  to <- getSourcePos
  return . cofree $ (mkPos from to) :< fold

pScan = do
  let comma = symbol ","
  from <- getSourcePos
  "scan"
  scan <- parens $ ScanF <$> (pIdent <* comma) <*> (pExpr <* comma) <*> pExpr
  to <- getSourcePos
  return . cofree $ (mkPos from to) :< scan

pTranspose = do
  from <- getSourcePos
  "transpose"
  tr <- parens $ TransposeF <$> (pExpr <* comma) <*> pPerm
  to <- getSourcePos
  return . cofree $ (mkPos from to) :< tr
  where
    comma = symbol ","
    pPerm =
      between (symbol "[") (symbol "]") $
        sepBy (fmap fromIntegral integer) comma

pApp :: Parser ExprSrc
pApp = do
  from <- getSourcePos
  funcName <- pIdent
  args <- parens $ pExpr `sepBy` symbol ","
  to <- getSourcePos
  case funcName of
    "gather" -> case args of
      [xs, is] -> return . cofree $ (mkPos from to) :< GatherF xs is
      _ -> fail "gather expects 2 arguments"
    "scatter_add" -> case args of
      [xs, is] -> return . cofree $ (mkPos from to) :< ScatterAddF xs is
      _ -> fail "scatter_add expects 2 arguments"
    "fold" -> fail "bad fold"
    "scan" -> fail "bad scan"
    "transpose" -> fail "bad transpose"
    _ -> return . cofree $ (mkPos from to) :< FunAppF funcName args
