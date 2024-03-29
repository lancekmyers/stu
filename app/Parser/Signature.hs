module Parser.Signature where

import AST (Bijector)
import Data.Text (Text)
import Parser.Bijectors (pBij)
import Parser.Types (pTy)
import Parser.Util
  ( Parser,
    lexeme,
    pIdent,
    pIdentUpper,
    parens,
    symbol,
  )
import Text.Megaparsec (MonadParsec (eof), many, sepBy, (<|>))
import Types (FunctionTy (..), Ty)

pArg :: Parser (Text, Ty)
pArg = do
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  return (name, ty)

-- >>> runParser pFunSig "" "fun sin(x: []real): []real;"
-- Right ("sin",[("x",[]real)],[]real)
-- >>> runParser pFunSig "" "fun matmul(a: ['m, 'n]real, b: ['n, 'o]real): ['m, 'o]real;"
-- Right ("matmul",[("a",['m,'n]real),("b",['n,'o]real)],['m,'o]real)
pFunSig :: Parser (Text, FunctionTy)
pFunSig = do
  symbol "fun"
  name <- lexeme pIdent
  args <- parens $ sepBy pArg (symbol ",")
  symbol ":"
  ret <- pTy
  symbol ";"
  return $ (name, FunctionTy args ret)

-- >>> runParser pDistSig "" "dist Exponential(lambda: []real): []real via SoftPlus();"
-- No instance for (Show
--                    (BijectorF (CofreeT BijectorF Identity SourcePos)))
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a6a4s
pDistSig :: Parser (Text, FunctionTy, Bijector ())
pDistSig = do
  symbol "dist"
  name <- lexeme pIdentUpper
  args <- parens $ sepBy pArg (symbol ",")
  symbol ":"
  ret <- pTy
  symbol "via"
  bij <- pBij
  symbol ";"
  return $ (name, FunctionTy args ret, const () <$> bij)

parseSignatures :: Parser [Either (Text, FunctionTy) (Text, FunctionTy, Bijector ())]
parseSignatures = many ((Left <$> pFunSig) <|> (Right <$> pDistSig)) <* eof
