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
import Util (SrcSpan)

-- parsing Bijectors
pBijNamed :: Parser (Bijector SrcSpan)
pBijNamed = do
  from <- getSourcePos
  bijName <- pIdentUpper
  bijArgs <- parens $ (lexeme L.float) `sepBy` symbol ","
  to <- getSourcePos
  return . cofree $ (from, to) :< (MkBij bijName bijArgs)

pBijChain :: Parser (Bijector SrcSpan)
pBijChain = do
  from <- getSourcePos
  symbol "Chain"
  bijs <- between (symbol "[") (symbol "]") $ pBij `sepBy` symbol ","
  to <- getSourcePos 
  return . cofree $ (from, to) :< (Chain bijs)

pBij :: Parser (Bijector SrcSpan)
pBij = pBijChain <|> pBijNamed
