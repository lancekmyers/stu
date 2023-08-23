module Parser.Bijectors where

import AST (Bijector(..), Parsing)
import Parser.Util (Parser, lexeme, pIdentUpper, parens, symbol)
import Text.Megaparsec
  ( SourcePos,
    between,
    getSourcePos,
    sepBy,
    (<|>),
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Util (SrcSpan, mkPos)

-- parsing Bijectors
pBijNamed :: Parser (SrcSpan, Bijector)
pBijNamed = do
  from <- getSourcePos
  bijName <- pIdentUpper
  bijArgs <- parens $ (lexeme L.float) `sepBy` symbol ","
  to <- getSourcePos
  return $ (mkPos from to, MkBij bijName bijArgs)

pBijChain :: Parser (SrcSpan, Bijector)
pBijChain = do
  from <- getSourcePos
  symbol "Chain"
  bijs <- fmap snd <$> 
    (between (symbol "[") (symbol "]") $ pBij `sepBy` symbol ",")
  to <- getSourcePos 
  return $ (mkPos from to, Chain bijs)

pBij :: Parser (SrcSpan, Bijector)
pBij = pBijChain <|> pBijNamed
