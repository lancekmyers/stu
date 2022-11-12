module Parser.Types (pTy) where

import AST
import Control.Monad.Combinators.Expr
import Control.Comonad.Trans.Cofree (Cofree(..), CofreeF(..), cofree)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

import Parser.Expr 
import Parser.Util

pCard :: Parser Card
pCard = choice [
  CardN . fromInteger <$> integer,
  CardFV <$> lexeme pIdentUpper,
  CardBV <$> (char' '\'' *> pIdent)
  ]

pREAL :: Parser ElTy
pREAL = symbol "real" >> pure REAL

pINT :: Parser ElTy
pINT = symbol "int" >> pure INT

pIND :: Parser ElTy
pIND = IND <$> pCard

pElTy :: Parser ElTy
pElTy = choice [pREAL, pINT, pIND]

pShape :: Parser Shape 
pShape = do 
  cards <-
    between (symbol "[") (symbol "]") $
      pCard `sepBy` symbol ","
  let shape = MkShape (V.fromList cards)
  return shape

pTy :: Parser Ty
pTy = do
  shape <- pShape
  el_ty <- pElTy
  return $ Ty shape el_ty
