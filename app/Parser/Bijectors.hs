module Parser.Bijectors where 

import Control.Comonad.Trans.Cofree (Cofree(..), CofreeF(..), cofree)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Util
import AST ( Bijector, BijectorF(..) )
import Data.Text (Text)

-- parsing Bijectors 
pBijNamed :: Parser (Bijector SourcePos)
pBijNamed = do 
  loc <- getSourcePos
  bijName <- pIdentUpper
  bijArgs <- parens $ (lexeme L.float) `sepBy` symbol ","
  return . cofree $ loc :< (MkBij bijName bijArgs)

pBijChain :: Parser (Bijector SourcePos)
pBijChain = do 
  loc <- getSourcePos 
  symbol "Chain"
  bijs <- between (symbol "[") (symbol "]") $ pBij `sepBy` symbol ","
  return . cofree $ loc :< (Chain bijs)

pBij :: Parser (Bijector SourcePos)
pBij = pBijChain <|> pBijNamed
