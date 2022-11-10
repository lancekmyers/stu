module Parser.Util where 

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text


sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

pIdent :: Parser Text
pIdent =
  T.pack
    <$> ((:) <$> lowerChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

pIdentUpper :: Parser Text
pIdentUpper =
  T.pack
    <$> ((:) <$> upperChar <*> many alphaNumChar <?> "identifier")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
