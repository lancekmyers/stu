module Parser.Distribution (pDistribution) where

import AST (Distribution (..))
import Parser.Expr (pExpr)
import Parser.Util (Parser, pIdentUpper, parens, symbol)
import Text.Megaparsec (SourcePos, getSourcePos, sepBy)

pDistribution :: Parser (Distribution SourcePos)
pDistribution = do
  loc <- getSourcePos
  distName <- pIdentUpper
  distArgs <- parens $ pExpr `sepBy` symbol ","
  return $ Distribution distName distArgs loc (Nothing, Nothing)
