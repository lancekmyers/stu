module Parser.FunDef (pFunDef, pFunBody) where 

import Text.Megaparsec ( choice, sepBy, SourcePos )
import Text.Megaparsec.Char ( char )
import Parser.Util ( lexeme, pIdent, parens, symbol, Parser )
import Parser.Types ( pTy )
import Types ( Ty ) 
import AST ( FunBody(..), FunDef(FunDef), PrimApp(..) ) 
import Data.Text (Text)
import Parser.Expr (pExpr)

-- >>> runParser pArg "" "x : []real"
-- Right ("x",[]real)
pArg :: Parser (Text, Ty)
pArg = do 
    name <- lexeme pIdent 
    symbol ":"
    ty <- pTy 
    return (name, ty)

-- >>> const () <$> runParser pFunDef "" "fun foo(): []real\nbegin \n\tret 4; end"
-- Right ()
pFunDef :: Parser (FunDef SourcePos)
pFunDef = do 
    symbol "fun"
    name <- lexeme pIdent
    args <- parens $ sepBy pArg (symbol ",") 
    symbol ":" 
    ret <- pTy
    symbol "begin"
    {- should the following be indentation sensitive? -}
    body <- pFunBody 
    symbol "end"
    return $ FunDef name args ret body 

pFunBody :: Parser (FunBody SourcePos) 
pFunBody = choice [pLetIn, pLetPrimIn, pRet] 

-- >>> fmap (const ()) $ runParser pLetIn "" "let x : []real = sin(x); ret x;"
-- Right ()
pLetIn :: Parser (FunBody SourcePos)
pLetIn = do 
    symbol "let"
    name <- lexeme pIdent 
    lexeme $ symbol ":" 
    ty <- pTy
    lexeme $ symbol "=" 
    val <- pExpr 
    lexeme $ symbol ";"
    rest <- pFunBody 
    return (FunLetIn name ty val rest)

-- >>> fmap (const ()) $ runParser pLetPrimIn "" "plet x : []real = %sin(x); ret x;"
-- Right ()
pLetPrimIn :: Parser (FunBody SourcePos)
pLetPrimIn = do 
    symbol "%let"
    name <- lexeme pIdent 
    lexeme $ symbol ":" 
    ty <- pTy
    lexeme $ symbol "=" 
    val <- pPrimApp 
    lexeme $ symbol ";"
    rest <- pFunBody 
    return (LetPrimIn name ty val rest)

pRet :: Parser (FunBody SourcePos)
pRet = do 
    symbol "ret"
    val <- pExpr 
    symbol ";"
    return $ FunRet val 

-- >>> const () <$> runParser pPrimApp "" "%sin(x, y)"
-- Right ()
pPrimApp :: Parser (PrimApp SourcePos)
pPrimApp = do 
    char '%'
    name <- pIdent `sepBy` (symbol ".")
    args <- parens $ (pExpr `sepBy` (symbol ","))
    return $ PrimApp name args
