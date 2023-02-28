module Parser.Types (pTy) where

import qualified Data.Vector as V
import Parser.Util
  ( Parser,
    integer,
    lexeme,
    pIdent,
    pIdentUpper,
    symbol,
  )
import Text.Megaparsec (between, choice, sepBy, getSourcePos)
import Text.Megaparsec.Char (char')
import Types
  ( Card (..),
    ElTy (..),
    Shape (MkShape),
    Ty (Ty),
  )
import Util (mkPos)

pCard :: Parser Card
pCard =
  choice
    [ CardN . fromInteger <$> integer,
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
  start <- getSourcePos
  shape <- pShape
  el_ty <- pElTy
  stop <- getSourcePos 
  return $ Ty shape el_ty (Just (mkPos start stop))
