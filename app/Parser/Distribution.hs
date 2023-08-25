module Parser.Distribution (pDistribution) where

import AST (Distribution (..), Parsing)
import Parser.Expr (pExpr)
import Parser.Util (Parser, pIdentUpper, parens, symbol)
import Text.Megaparsec (SourcePos, getSourcePos, sepBy)
import Util (SrcSpan, mkPos)

pDistribution :: Parser (Distribution Parsing)
pDistribution = do
  from <- getSourcePos
  distName <- pIdentUpper
  distArgs <- parens $ pExpr `sepBy` symbol ","
  to <- getSourcePos 
  return $ Distribution distName distArgs (mkPos from to) (Nothing, Nothing)
