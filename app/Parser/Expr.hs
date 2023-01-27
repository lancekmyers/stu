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

pVariable :: Parser ExprSrc
pVariable = do
  position <- getSourcePos
  ident <- lexeme pIdent
  return . cofree $ position :< VarF ident Unknown

pLitInt :: Parser ExprSrc
pLitInt = do
  pos <- getSourcePos
  int <- signedInteger
  let litInt = LitInt . fromInteger $ int
  return . cofree $ pos :< litInt

pLitReal :: Parser ExprSrc
pLitReal = do

  pos <- getSourcePos
  lit <- LitReal <$> signedFloat
  return . cofree $ pos :< lit

pLitArray :: Parser ExprSrc
pLitArray = do
  pos <- getSourcePos
  litArr <-
    between (symbol "[") (symbol "]") $
      LitArray <$> sepBy pExpr (symbol ",")
  return . cofree $ pos :< litArr

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
      try pApp,
      try pVariable,
      pLit
    ]

pExpr :: Parser ExprSrc
pExpr = makeExprParser pTerm operatorTable

pAdd = do
  loc <- getSourcePos
  star <- symbol "+"
  return $ \x y -> cofree $ loc :< ArithF Add x y

pMul = do
  loc <- getSourcePos
  star <- symbol "*"
  return $ \x y -> cofree $ loc :< ArithF Mul x y

pDiv = do
  loc <- getSourcePos
  star <- symbol "/"
  return $ \x y -> cofree $ loc :< ArithF Div x y

pSub = do
  loc <- getSourcePos
  star <- symbol "-"
  return $ \x y -> cofree $ loc :< ArithF Sub x y


operatorTable :: [[Operator Parser ExprSrc]]
operatorTable =
  [ [ InfixL pMul,
      InfixL pDiv
    ],
    [ InfixL pAdd,
      InfixL pSub
    ]
  ]

pApp :: Parser ExprSrc
pApp = do
  loc <- getSourcePos
  funcName <- pIdent
  args <- parens $ pExpr `sepBy` symbol ","
  case funcName of
    "gather" -> case args of
      [xs, is] -> return . cofree $ loc :< GatherF xs is
      _ -> fail "gather expects 2 arguments"
    _ -> return . cofree $ loc :< FunAppF funcName args
