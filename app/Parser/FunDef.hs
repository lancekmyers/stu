module Parser.FunDef (pFunDef, pFunBody) where

import AST (FunBody (..), FunDef (FunDef), PrimApp (..))
import Data.Text (Text)
import Parser.Expr (pExpr)
import Parser.Types (pTy)
import Parser.Util (Parser, lexeme, pIdent, parens, symbol)
import Text.Megaparsec (SourcePos, choice, sepBy, (<?>), getSourcePos)
import Text.Megaparsec.Char (char)
import Types (Ty)
import Util (SrcSpan)

-- >>> runParser pArg "" "x : []real"
-- Right ("x",[]real)
pArg :: Parser (Text, Ty)
pArg = do
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  return (name, ty)

-- >>> const () <$> runParser pFunDef "" "fun foo(): []real\nbegin \n\tret 4; end"
-- Right ()
pFunDef :: Parser (FunDef SrcSpan)
pFunDef = do
  symbol "fun"
  name <- lexeme pIdent
  args <- parens $ sepBy pArg (symbol ",")
  ret <- (symbol ":" *> pTy) <?> "Type Annotation"
  symbol "begin"
  {- should the following be indentation sensitive? -}
  body <- pFunBody
  symbol "end"
  return $ FunDef name args ret body

pFunBody :: Parser (FunBody SrcSpan)
pFunBody = choice [pLetIn, pLetPrimIn, pRet]

-- >>> fmap (const ()) $ runParser pLetIn "" "let x : []real = sin(x); ret x;"
-- Right ()
pLetIn :: Parser (FunBody SrcSpan)
pLetIn = do
  symbol "let"
  name <- lexeme pIdent
  lexeme $ symbol ":"
  ty <- pTy
  lexeme $ symbol "="
  val <- pExpr
  lexeme $ symbol ";"
  rest <- pFunBody
  return (FunLetIn name ty val rest)

-- >>> fmap (const ()) $ runParser pLetPrimIn "" "plet x : []real = %sin(x); ret x;"
-- Right ()
pLetPrimIn :: Parser (FunBody SrcSpan)
pLetPrimIn = do
  symbol "%let"
  name <- lexeme pIdent
  lexeme $ symbol ":"
  ty <- pTy
  lexeme $ symbol "="
  val <- pPrimApp
  lexeme $ symbol ";"
  rest <- pFunBody
  return (LetPrimIn name ty val rest)

pRet :: Parser (FunBody SrcSpan)
pRet = do
  symbol "ret"
  val <- pExpr
  symbol ";"
  return $ FunRet val

-- >>> const () <$> runParser pPrimApp "" "%sin(x, y)"
-- Right ()
pPrimApp :: Parser (PrimApp SrcSpan)
pPrimApp = do
  char '%'
  name <- pIdent `sepBy` (symbol ".")
  args <- parens $ (pExpr `sepBy` (symbol ","))
  return $ PrimApp name args
