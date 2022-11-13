module Parser.Distribution (pDistribution) where 

import Text.Megaparsec ( SourcePos, sepBy, getSourcePos )
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Util ( pIdentUpper, parens, symbol, Parser )
import Parser.Types ( pTy )
import Parser.Bijectors ( pBij )
import Types ( Ty, FunctionTy(..) ) 
import AST ( Distribution(..) ) 
import Data.Text (Text)
import Parser.Expr (pExpr)

pDistribution :: Parser (Distribution SourcePos)
pDistribution = do
  loc <- getSourcePos
  distName <- pIdentUpper
  distArgs <- parens $ pExpr `sepBy` symbol ","
  return $ Distribution distName distArgs loc (Nothing, Nothing)

