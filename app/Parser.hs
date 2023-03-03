module Parser (parseProgram, parseSignatures, parseLibrary) where

import AST
  ( Decl (..),
    DistDef,
    FunDef,
    Library (Library),
    Model (..),
    ModelStmt (..),
    Program (Program),
  )
import Data.Text (Text)
import Parser.Bijectors (pBij)
import Parser.DistDef (pDistDef)
import Parser.Distribution (pDistribution)
import Parser.Expr (pExpr)
import Parser.FunDef (pFunDef)
import Parser.Signature (parseSignatures)
import Parser.Types (pTy)
import Parser.Util (Parser, lexeme, pIdent, pIdentUpper, symbol)
import Text.Megaparsec
  ( MonadParsec (eof),
    choice,
    many,
    optional,
    (<|>),
  )
import Util (SrcSpan)


semi :: Parser Text
semi = symbol ";"

sym :: Parser Text
sym = symbol "~"

pCardDecl :: Parser Decl
pCardDecl = do
  symbol "card"
  name <- lexeme pIdentUpper
  semi
  return $ CardDecl name

pFactorDecl :: Parser Decl
pFactorDecl = do
  symbol "factor"
  name <- lexeme pIdentUpper
  semi
  return $ FactorDecl name

pDataDecl :: Parser Decl
pDataDecl = do
  symbol "data"
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  semi
  return $ DataDecl name ty

pValStmt :: Parser (ModelStmt SrcSpan)
pValStmt = do
  symbol "val"
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  symbol "="
  val <- pExpr
  semi
  return $ ValStmt name ty val

pParamStmt :: Parser (ModelStmt SrcSpan)
pParamStmt = do
  symbol "param"
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  sym
  dist <- pDistribution
  bij <- optional (symbol "via" >> pBij)
  semi
  return $ ParamStmt name ty dist bij

pObsStmt :: Parser (ModelStmt SrcSpan)
pObsStmt = do
  symbol "obs"
  name <- lexeme pIdent
  sym
  dist <- pDistribution
  semi
  return $ ObsStmt name dist

pDecl :: Parser Decl
pDecl = choice [pCardDecl, pFactorDecl, pDataDecl]

pModelStmt :: Parser (ModelStmt SrcSpan)
pModelStmt = choice [pValStmt, pParamStmt, pObsStmt]

pModel :: Parser (Model SrcSpan)
pModel = Model <$> many pModelStmt


-----

-- parsing programs

parseProgram :: Parser (Program SrcSpan)
parseProgram = do
  decls <- many pDecl
  model <- pModel
  eof
  return $ Program decls model

parseLibrary :: Parser (Library SrcSpan)
parseLibrary = do
  defs <- many $ (Left <$> pFunDef) <|> (Right <$> pDistDef)
  eof
  return $ foldr go (Library [] []) defs
  where
    go :: Either (FunDef a) (DistDef a) -> Library a -> Library a
    go def (Library funs dists) = case def of
      Left fdef -> Library (fdef : funs) dists
      Right ddef -> Library funs (ddef : dists)
