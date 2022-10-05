{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import AST
import Analysis
import CodeGen (writeProg, cgModel)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, guard)
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Parser (parseProgram)
import Prettyprinter
import Prettyprinter.Render.Text (putDoc)
import System.Console.ANSI.Codes -- (clearFromCursorToScreenBeginningCode)
import System.Directory
import System.Environment (getArgs)
import System.FSNotify
import System.FilePath
import qualified Text.Builder as B
import Text.Megaparsec (errorBundlePretty, runParser, SourcePos)
import Types
import Options.Applicative

data Options = Options {
  inFileName :: FilePath, 
  outFileName :: Maybe FilePath
}

options :: Parser Options 
options = Options 
      <$> strOption
          ( long "model"
         <> metavar "TARGET"
         <> help "File containing model to compile" )
      <*> optional (strOption 
          ( long "output" 
          <> short 'o'
          <> metavar "OUTPUT"
          <> help "File to write python model to" )
        )
      


{-
   "         __       "
   "  _____/ /___  __ "
   " / ___/ __/ / / / "
   "(__  ) /_/ /_/ /  "
   "/____/\\__/\\__,_/"
-}
type Err = Doc ()

getFileName :: ExceptT Err IO String
getFileName = do
  args <- lift getArgs
  case args of
    [] -> throwError "No file name provided"
    [fname] -> return fname
    _  -> throwError "To many files provided"

parseFile :: FilePath -> ExceptT Err IO (Program SourcePos)
parseFile fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseProgram fname fcontents of
    Left err -> throwError . pretty $ errorBundlePretty err
    Right (decls, m) -> return $ Program decls m

checkProgram :: Monad m => Program SourcePos -> ExceptT Err m (Program Ty)
checkProgram (Program decls model) = do 
  let ctx = buildCtx decls
  (model, ctx) <- withExceptT pretty $ runStateT (checkModel model) ctx
  return $ Program decls model 

validateFileNames :: Options -> ExceptT Err IO (FilePath, FilePath) 
validateFileNames opts = do 
  let fname = inFileName opts
  let out_fname = fromMaybe (fname -<.> ".py") (outFileName opts)
  
  case takeExtension fname of
    ".stu" -> pure ()
    ext -> throwError $ "Expected a '.stu' file, got: '" <> pretty ext <> "'"

  file_exists <- liftIO $ doesFileExist fname
  when (not file_exists) (throwError $ "File does not exist")

  return (fname, out_fname)

main' :: Options -> ExceptT Err IO ()
main' opts = do 
  (fname, out_fname) <- validateFileNames opts

  liftIO . putStrLn $ "Compiling " ++ fname
  
  prog <- parseFile fname >>= checkProgram
  
  let py_src = B.run $ writeProg prog
  lift $ TIO.writeFile out_fname py_src  
  liftIO . putStrLn $ "Compiled model written to " ++ out_fname

main :: IO ()
main = mainHandled =<< execParser opts
  where
    mainHandled opts =
      runExceptT (main' opts) >>= \case 
        Left err -> print err
        Right foo -> return ()
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Compile a stu model"
     <> header "stu" )


buildCtx :: [Decl] -> Ctx
buildCtx decls = Ctx vars funs dists knownCards varDoms
  where
    vars :: Map Text Ty
    vars = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, ty)
        go _ = Nothing
    varDoms = M.fromList $ mapMaybe go decls
      where
        go (DataDecl name ty) = Just (name, Data)
        go _ = Nothing

    scalarFunTy = FunctionTy 0 [("x", Ty [] REAL)] (Ty [] REAL)
    funs :: Map Text FunctionTy
    funs =
      M.fromList
        [ ("sin", scalarFunTy),
          ("cos", scalarFunTy),
          ("tan", scalarFunTy),
          ("exp", scalarFunTy),
          ("ln", scalarFunTy),
          ("sqrt", scalarFunTy)
        ]
    real = Ty [] REAL
    locScale = FunctionTy 0 [("loc", real), ("scale", real)] real
    dists :: Map Text FunctionTy
    dists =
      M.fromList
        [ ("Normal", locScale),
          ("HalfNormal", locScale),
          ("Bernoulli", FunctionTy 0 [("prob", real)] (Ty [] INT)),
          ("Beta", FunctionTy 0 [("alpha", real), ("beta", real)] (real)),
          ("Gamma", 
            FunctionTy 0 [("concentration", real), ("rate", real)] (real)),
          ("MVNormal", FunctionTy 1 
            [ ("mu", Ty [CardBV 0] REAL)
            , ("sigma", Ty [CardBV 0, CardBV 0] REAL)
            ]
            real),
          ("MVNormal", FunctionTy 1 
            [ ("mu", Ty [CardBV 0] REAL)
            , ("sigma", Ty [CardBV 0] REAL)
            ] 
            (Ty [CardBV 0] REAL))
        ]

    knownCards :: Set Text
    knownCards = S.fromList $ mapMaybe go decls
      where
        go (CardDecl name) = Just name
        go (FactorDecl name) = Just name
        go _ = Nothing
