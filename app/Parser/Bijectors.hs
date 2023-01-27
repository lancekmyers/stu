module Parser.Bijectors where

import AST (Bijector, BijectorF (..))
import Control.Comonad.Trans.Cofree (CofreeF ((:<)), cofree)
import Parser.Util (Parser, lexeme, pIdentUpper, parens, symbol)
import Text.Megaparsec
  ( SourcePos,
    between,
    getSourcePos,
    sepBy,
    (<|>),
  )
import qualified Text.Megaparsec.Char.Lexer as L

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
