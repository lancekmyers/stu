module Parser.Distribution (pDistribution) where 

import Text.Megaparsec ( SourcePos, sepBy, getSourcePos )
import Parser.Util ( pIdentUpper, parens, symbol, Parser ) 
import AST ( Distribution(..) ) 
import Parser.Expr (pExpr)

pDistribution :: Parser (Distribution SourcePos)
pDistribution = do
  loc <- getSourcePos
  distName <- pIdentUpper
  distArgs <- parens $ pExpr `sepBy` symbol ","
  return $ Distribution distName distArgs loc (Nothing, Nothing)

