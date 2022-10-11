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
import Prettyprinter.Render.Terminal (AnsiStyle, putDoc, bold, color, Color(..))
import System.Directory ( doesFileExist )
import System.FilePath
import qualified Text.Builder as B
import Text.Megaparsec (errorBundlePretty, runParser, SourcePos)
import Types
import Options.Applicative

data Options
  = BuildOptions 
    { inFileName' :: FilePath
    , outFileName' :: Maybe FilePath  
    }
  | CheckOptions 
    { inFileName' :: FilePath }

options :: Parser Options
options = subparser $ 
  (command "build" (info buildOptions $ progDesc "Build a stu model"))
  <> (command "check" (info checkOptions $ progDesc "Check a stu model without building"))
  where 
    buildOptions = BuildOptions 
      <$> (argument str (metavar "MODEL" <> help "File containing model to compile"))
      <*> optional ( strOption (long "output" 
            <> short 'o'
            <> metavar "OUTPUT"
            <> help "File to write python model to" )
          )
    checkOptions = CheckOptions 
      <$> (argument str (metavar "MODEL" <> help "File containing model to compile"))

{-
   "         __       "
   "  _____/ /___  __ "
   " / ___/ __/ / / / "
   "(__  ) /_/ /_/ /  "
   "/____/\\__/\\__,_/"
-}
type Err = Doc AnsiStyle


parseFile :: FilePath -> ExceptT Err IO (Program SourcePos)
parseFile fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseProgram fname fcontents of
    Left err -> throwError . pretty $ errorBundlePretty err
    Right (decls, m) -> return $ Program decls m

checkProgram :: Monad m => Program SourcePos -> ExceptT Err m (Program Ty)
checkProgram (Program decls model) = do 
  let ctx = buildCtx decls
  (model, ctx) <- withExceptT prettyError $ runStateT (checkModel model) ctx
  return $ Program decls model 

validateFileNames :: (FilePath, Maybe FilePath) -> ExceptT Err IO (FilePath, FilePath) 
validateFileNames (inFileName, outFileName) = do 
  let fname = inFileName 
  let out_fname = fromMaybe (fname -<.> ".py") outFileName
  
  case takeExtension fname of
    ".stu" -> pure ()
    ext -> throwError $ "Expected a '.stu' file, got: '" <> pretty ext <> "'"

  file_exists <- liftIO $ doesFileExist fname
  when (not file_exists) (throwError $ "File does not exist")

  return (fname, out_fname)

main' :: Options -> ExceptT Err IO ()
main' (BuildOptions inFileName outFileName) = do 
  (fname, out_fname) <- validateFileNames (inFileName, outFileName)

  liftIO . putDoc $ "Checking  " <> (annotate bold . pretty $ fname) <> line
  
  prog <- parseFile fname >>= checkProgram
  
  liftIO . putDoc $ (annotate (color Green) $ "Success") <+> 
    "checked" <+> pretty fname <> line

  let py_src = B.run $ writeProg prog
  lift $ TIO.writeFile out_fname py_src  
  liftIO . putDoc $ indent 2 $ "compiled output written to" <+> pretty out_fname
main' (CheckOptions inFileName) = do 
  (fname, _) <- validateFileNames (inFileName, Nothing)

  liftIO . putDoc $ "Checking  " <> (annotate bold . pretty $ fname) <> line
  
  prog <- parseFile fname >>= checkProgram  
  
  liftIO . putDoc $ (annotate (color Green) $ "Success") <+> 
    "checked" <+> pretty fname <> line

main :: IO ()
main = mainHandled =<< execParser opts
  where
    mainHandled opts =
      runExceptT (main' opts) >>= \case 
        Left err -> putDoc err
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
        [ ("sin",  scalarFunTy),
          ("cos",  scalarFunTy),
          ("tan",  scalarFunTy),
          ("exp",  scalarFunTy),
          ("ln",   scalarFunTy),
          ("sqrt", scalarFunTy), 
          ("mean", FunctionTy 1 [("x", Ty [CardBV 0] REAL)] real)
        ]
    real = Ty [] REAL
    locScale = FunctionTy 0 [("loc", real), ("scale", real)] real
    dists :: Map Text FunctionTy
    dists =
      M.fromList
        [ ("Normal", locScale),
          ("HalfNormal", locScale),
          ("Bernoulli", FunctionTy 0 [("prob", real)] (Ty [] INT)),
          ("Poisson", FunctionTy 0 [("mu", real)] (Ty [] INT)),
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
