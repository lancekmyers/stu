module Parser.Signature where 

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Util
import Parser.Types ( pTy )
import Parser.Bijectors ( pBij )
import Types ( Ty ) 
import AST ( Bijector )
import Data.Text (Text)


pArg :: Parser (Text, Ty)
pArg = do 
    name <- pIdent 
    symbol ":"
    ty <- pTy 
    return (name, ty)

-- >>> runParser pFunSig "" "fun sin(x: []real): []real;"
-- Right ("sin",[("x",[]real)],[]real)
pFunSig :: Parser (Text, [(Text, Ty)], Ty)
pFunSig = do
  symbol "fun"
  name <- lexeme pIdent
  args <- parens $ sepBy pArg (symbol ",") 
  symbol ":" 
  ret <- pTy
  symbol ";"
  return $ (name, args, ret)

-- >>> runParser pDistSig "" "dist Exponential(lambda: []real): []real via SoftPlus();"
-- No instance for (Show
--                    (BijectorF (CofreeT BijectorF Identity SourcePos)))
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_aMiS
pDistSig :: Parser (Text, [(Text, Ty)], Ty, Bijector SourcePos)
pDistSig = do
  symbol "dist"
  name <- lexeme pIdentUpper
  args <- parens $ sepBy pArg (symbol ",") 
  symbol ":" 
  ret <- pTy
  symbol "via"
  bij <- pBij
  symbol ";"
  return $ (name, args, ret, bij)

