{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Parser.DistDef where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Util
import Parser.Types ( pTy )
import Parser.Bijectors ( pBij )
import Types ( Ty(..), FunctionTy(..), ElTy (..) ) 
import AST ( PrimApp(..), FunBody(..), DistDef(..), FunDef(..), SampleBody (..) ) 
import Data.Text (Text)
import Parser.Expr (pExpr)
import Parser.FunDef 
import Parser.Distribution


-- >>> runParser pArg "" "x : []real"
-- Right ("x",[]real)
pArg :: Parser (Text, Ty)
pArg = do 
    name <- lexeme pIdent 
    symbol ":"
    ty <- pTy 
    return (name, ty)

pDistDef :: Parser (DistDef SourcePos)
pDistDef = do 
    symbol "dist"
    name <- lexeme pIdentUpper
    args <- parens $ sepBy pArg (symbol ",") 
    symbol ":" 
    eventTy <- pTy
    symbol "begin"    
    lpdf <- pLPDF args eventTy 
    sampler <- pSample
    bij <- pBij
    symbol "end"

    return $ DistDef name args eventTy lpdf sampler bij
    
pLPDF :: [(Text, Ty)] -> Ty -> Parser (FunDef SourcePos)
pLPDF args ty = do 
    symbol "lpdf" 
    name <- parens pIdent 
    symbol "begin"
    body <- pFunBody
    symbol "end"
    return $ FunDef name ((name, ty) : args) (Ty [] REAL) body 

pSample :: Parser (SampleBody SourcePos)
pSample = do 
    symbol "sample"
    body <- pSampleBody
    symbol "end"
    return body 

pSampleBody :: Parser (SampleBody SourcePos)
pSampleBody = choice ([pLetIn, pSampleIn, pRet, pSampleUnif] :: [_])

pLetIn :: Parser (SampleBody SourcePos)
pLetIn = do 
    symbol "let"
    name <- lexeme pIdent 
    lexeme $ symbol ":" 
    ty <- pTy
    lexeme $ symbol "=" 
    val <- pExpr 
    lexeme $ symbol ";"
    rest <- pSampleBody 
    return (SampleLetIn name ty val rest)

pSampleIn :: Parser (SampleBody SourcePos)
pSampleIn = do 
    symbol "gen"
    name <- lexeme pIdent 
    lexeme $ symbol ":" 
    ty <- pTy
    lexeme $ symbol "~" 
    dist <- pDistribution 
    lexeme $ symbol ";"
    rest <- pSampleBody
    return $ SampleIn name ty dist rest 

pSampleUnif :: Parser (SampleBody SourcePos)
pSampleUnif = do 
    symbol "unif"
    name <- lexeme pIdent 
    lexeme $ symbol ":" 
    ty <- pTy
    lexeme $ symbol ";"
    rest <- pSampleBody
    return $ SampleUnifIn name ty rest 

pRet :: Parser (SampleBody SourcePos)
pRet = do 
    symbol "ret"
    val <- pExpr 
    symbol ";"
    return $ SampleRet val 
