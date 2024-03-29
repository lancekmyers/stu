module Parser.Distribution (pDistribution) where

import AST (Distribution (..))
import Parser.Expr (pExpr)
import Parser.Util (Parser, pIdentUpper, parens, symbol)
import Text.Megaparsec (SourcePos, getSourcePos, sepBy)
import Util (SrcSpan, mkPos)

pDistribution :: Parser (Distribution SrcSpan)
pDistribution = do
  from <- getSourcePos
  distName <- pIdentUpper
  distArgs <- parens $ pExpr `sepBy` symbol ","
  to <- getSourcePos 
  return $ Distribution distName distArgs (mkPos from to) (Nothing, Nothing)
