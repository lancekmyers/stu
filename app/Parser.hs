module Parser (parseProgram) where

import AST
import Control.Comonad.Trans.Cofree (Cofree(..), CofreeF(..), cofree)
import Data.Text (Text)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Expr ( pExpr ) 
import Parser.Util
import Parser.Types ( pTy )
import Parser.Signature (pArg)
import Parser.Bijectors (pBij)


pDistribution :: Parser (Distribution SourcePos)
pDistribution = do
  loc <- getSourcePos
  distName <- pIdentUpper
  distArgs <- parens $ pExpr `sepBy` symbol ","
  return $ Distribution distName distArgs loc (Nothing, Nothing)

------

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

pValStmt :: Parser (ModelStmt SourcePos)
pValStmt = do
  symbol "val"
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  symbol "="
  val <- pExpr
  semi
  return $ ValStmt name ty val

pParamStmt :: Parser (ModelStmt SourcePos)
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

pObsStmt :: Parser (ModelStmt SourcePos)
pObsStmt = do
  symbol "obs"
  name <- lexeme pIdent
  sym
  dist <- pDistribution
  semi
  return $ ObsStmt name dist

pDecl :: Parser Decl
pDecl = choice [pCardDecl, pFactorDecl, pDataDecl]

pModelStmt :: Parser (ModelStmt SourcePos)
pModelStmt = choice [pValStmt, pParamStmt, pObsStmt]

pModel :: Parser (Model SourcePos)
pModel = Model <$> many pModelStmt

-----

-- parsing programs

parseProgram :: Parser (Program SourcePos)
parseProgram = do
  decls <- many pDecl 
  model <- pModel 
  eof 
  return $ Program decls model
