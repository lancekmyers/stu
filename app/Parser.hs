module Parser where

-- (1)

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

-- Parsing Expressions
pVariable :: Parser ExprSrc
pVariable = do 
  position <- getSourcePos 
  ident <- lexeme pIdent
  return . cofree $ position :< VarF ident Unknown

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pLitInt :: Parser ExprSrc
pLitInt = do 
  pos <- getSourcePos 
  int <- signedInteger
  let litInt = LitInt . fromInteger $ int
  return . cofree $ pos :< litInt 

pLitReal :: Parser ExprSrc
pLitReal = do 
  pos <- getSourcePos
  lit <- LitReal <$> signedFloat
  return . cofree $ pos :< lit

pLitArray :: Parser ExprSrc
pLitArray = do 
  pos <- getSourcePos
  litArr <- between (symbol "[") (symbol "]") $
    LitArray <$> sepBy pExpr (symbol ",")
  return . cofree $ pos :< litArr

pLit :: Parser (ExprSrc)
pLit = choice 
  [ pLitArray,
    try pLitReal,
    pLitInt
  ]

pTerm :: Parser ExprSrc
pTerm =
  choice
    [ parens pExpr,
      try pApp,
      try pVariable,
      pLit
    ]

pExpr :: Parser ExprSrc
pExpr = makeExprParser pTerm operatorTable

pAdd = do 
  star <- symbol "+" 
  loc <- getSourcePos
  return $ \x y -> cofree $ loc :< ArithF Add x y  
pMul = do 
  star <- symbol "*" 
  loc <- getSourcePos
  return $ \x y -> cofree $ loc :< ArithF Mul x y  
pDiv = do 
  star <- symbol "/" 
  loc <- getSourcePos
  return $ \x y -> cofree $ loc :< ArithF Div x y  
pSub = do 
  star <- symbol "-" 
  loc <- getSourcePos
  return $ \x y -> cofree $ loc :< ArithF Sub x y  


operatorTable :: [[Operator Parser ExprSrc]]
operatorTable =
  [ [ InfixL pMul,
      InfixL pDiv
    ],
    [ InfixL pAdd,
      InfixL pSub
    ]
  ]

pApp :: Parser ExprSrc
pApp = do
  loc <- getSourcePos
  funcName <- pIdent
  args <- parens $ pExpr `sepBy` symbol ","
  case funcName of
    "gather" -> case args of
      [xs, is] -> return . cofree $ loc :< GatherF xs is
      _ -> fail "gather expects 2 arguments"
    _ -> return . cofree $ loc :< FunAppF funcName args

pDistribution :: Parser (Distribution SourcePos)
pDistribution = do
  loc <- getSourcePos
  distName <- pIdentUpper
  distArgs <- parens $ pExpr `sepBy` symbol ","
  return $ Distribution distName distArgs loc (Nothing, Nothing)

------

semi :: Parser Text
semi = symbol ";"

sym :: Parser Text
sym = symbol "~"

pCardDecl :: Parser Decl
pCardDecl = do
  symbol "card"
  name <- lexeme pIdentUpper
  semi
  return $ CardDecl name

pFactorDecl :: Parser Decl
pFactorDecl = do
  symbol "factor"
  name <- lexeme pIdentUpper
  semi
  return $ FactorDecl name

pDataDecl :: Parser Decl
pDataDecl = do
  symbol "data"
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  semi
  return $ DataDecl name ty

pValStmt :: Parser (ModelStmt SourcePos)
pValStmt = do
  symbol "val"
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  symbol "="
  val <- pExpr
  semi
  return $ ValStmt name ty val

pParamStmt :: Parser (ModelStmt SourcePos)
pParamStmt = do
  symbol "param"
  name <- lexeme pIdent
  symbol ":"
  ty <- pTy
  sym
  dist <- pDistribution
  bij <- optional (symbol "via" >> pBij)
  semi
  return $ ParamStmt name ty dist bij

pObsStmt :: Parser (ModelStmt SourcePos)
pObsStmt = do
  symbol "obs"
  name <- lexeme pIdent
  sym
  dist <- pDistribution
  semi
  return $ ObsStmt name dist

pDecl = choice [pCardDecl, pFactorDecl, pDataDecl]

pModelStmt = choice [pValStmt, pParamStmt, pObsStmt]

pModel = Model <$> many pModelStmt

----
pCard :: Parser Card
pCard = (CardN . fromInteger <$> integer) <|> (CardFV <$> lexeme pIdentUpper)

pREAL :: Parser ElTy
pREAL = symbol "real" >> pure REAL

pINT :: Parser ElTy
pINT = symbol "int" >> pure INT

pIND :: Parser ElTy
pIND = IND <$> pCard

pElTy :: Parser ElTy
pElTy = choice [pREAL, pINT, pIND]

pTy :: Parser Ty
pTy = do
  cards <-
    between (symbol "[") (symbol "]") $
      pCard `sepBy` symbol ","
  let shape = V.fromList cards
  el_ty <- pElTy
  return $ Ty shape el_ty

-- pTy :: Parser Ty
-- pTy = choice [ pReal, pInt, IND <$> pCard, pTensorTy]

-----
-- parsing Bijectors 
pBijNamed :: Parser (Bijector SourcePos)
pBijNamed = do 
  loc <- getSourcePos
  bijName <- pIdentUpper
  bijArgs <- parens $ (lexeme L.float) `sepBy` symbol ","
  return . cofree $ loc :< (MkBij bijName bijArgs)

pBijChain :: Parser (Bijector  SourcePos)
pBijChain = do 
  loc <- getSourcePos 
  symbol "Chain"
  bijs <- between (symbol "[") (symbol "]") $ pBij `sepBy` symbol ","
  return . cofree $ loc :< (Chain bijs)

pBij :: Parser (Bijector SourcePos)
pBij = pBijChain <|> pBijNamed

-----

-- parsing programs

parseProgram :: Parser ([Decl], Model SourcePos)
parseProgram = ((,) <$> many pDecl <*> pModel) <* eof
