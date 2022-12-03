{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import AST ( Library, Program(Program) )
import Analysis
    ( prettyError, Ctx, buildCtx, ctxFromSigs, checkModel, checkLib ) 
import CodeGen (writeProg, writeLib)
import Control.Monad.Except
    ( MonadError(throwError),
      when,
      MonadIO(liftIO),
      MonadTrans(lift),
      ExceptT,
      runExceptT,
      withExceptT )
import Control.Monad.State
    ( StateT(runStateT), when, MonadIO(liftIO), MonadTrans(lift) )
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Parser (parseProgram, parseSignatures, parseLibrary)
import Prettyprinter
    ( Pretty(pretty), Doc, (<+>), annotate, indent, line )
import Prettyprinter.Render.Terminal (AnsiStyle, putDoc, bold, color, Color(..))
import System.Directory ( doesFileExist )
import System.FilePath ( (-<.>), takeExtension )
import qualified Text.Builder as B
import Text.Megaparsec (errorBundlePretty, runParser, SourcePos)
import Types ( Ty )
import Control.Applicative (optional, (<**>))
import Options.Applicative
    ( Parser,
      argument,
      command,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      str,
      strOption,
      subparser,
      execParser,
      helper )
import Control.Monad.Reader (runReader)

data Options
  = BuildOptions 
    { inFileName' :: FilePath
    , outFileName' :: Maybe FilePath  
    }
  | CheckOptions 
    { inFileName' :: FilePath }
  | LibOptions FilePath

options :: Parser Options
options = subparser $ 
  (command "build" (info buildOptions $ progDesc "Build a stu model"))
  <> (command "check" (info checkOptions $ progDesc "Check a stu model without building"))
  <> (command "lib" (info libOptions $ progDesc "Check a stu library and compile"))
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

    libOptions = LibOptions 
      <$> (argument str (metavar "LIBRARY" <> help "File containing librar"))

    

{-
   "         __       "
   "  _____/ /___  __ "
   " / ___/ __/ / / / "
   "(__  ) /_/ /_/ /  "
   "/____/\\__/\\__,_/"
-}
type Err = Doc AnsiStyle


parseFile :: FilePath -> ExceptT Err IO (Text, Program SourcePos)
parseFile fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseProgram fname fcontents of
    Left err -> throwError . pretty $ errorBundlePretty err
    Right prog -> return $ (fcontents, prog)

parseLibFile :: FilePath -> ExceptT Err IO (Text, Library SourcePos)
parseLibFile fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseLibrary fname fcontents of
    Left err -> throwError . pretty $ errorBundlePretty err
    Right lib -> return $ (fcontents, lib)

parseSig :: FilePath -> ExceptT Err IO Ctx 
parseSig fname = do 
  fcontents <- lift $ TIO.readFile fname
  case runParser parseSignatures fname fcontents of
    Left err -> throwError . pretty $ errorBundlePretty err
    Right ctx -> return $ ctxFromSigs ctx 

checkProgram :: Monad m => Text -> Ctx -> Program SourcePos -> ExceptT Err m (Program Ty, Ctx)
checkProgram src ctx_std (Program decls model) = do 
  let ctx = ctx_std <> buildCtx decls
  (model, ctx') <- withExceptT (prettyError src) $ 
    runStateT (checkModel model) ctx
  return $ (Program decls model, ctx) 

checkLibrary:: Monad m => Text -> Library SourcePos -> ExceptT Err m (Library Ty, Ctx)
checkLibrary src lib = do 
  (lib', ctx') <- withExceptT (prettyError src) $ 
    runStateT (checkLib lib) mempty
  return $ (lib', ctx') 

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
  
  (src, prog) <- parseFile fname 
  std_lib <- parseSig "lib/std.stu"
  (progChecked, ctx) <- checkProgram src std_lib prog
  
  liftIO . putDoc $ (annotate (color Green) $ "Success") <+> 
    "checked" <+> pretty fname <> line

  let py_src = runReader (writeProg progChecked) ctx
  lift $ TIO.writeFile out_fname $ B.run py_src  
  liftIO . putDoc $ indent 2 $ "compiled output written to" <+> pretty out_fname
main' (CheckOptions inFileName) = do 
  (fname, _) <- validateFileNames (inFileName, Nothing)

  liftIO . putDoc $ "Checking  " <> (annotate bold . pretty $ fname) <> line
  
  (src, prog) <- parseFile fname 
  std_lib <- parseSig "lib/std.stu"
  progChecked <- checkProgram src std_lib prog
  
  liftIO . putDoc $ (annotate (color Green) $ "Success") <+> 
    "checked" <+> pretty fname <> line
main' (LibOptions libFile) = do
  (fname, out_fname) <- validateFileNames (libFile, Nothing)
  (src, lib) <- parseLibFile fname 
  (libChecked, ctx) <- checkLibrary src lib
  -- let _ = traverse checkFunDef (_funs prog)
  let py_src = runReader (writeLib libChecked) mempty
  lift $ TIO.writeFile out_fname $ B.run py_src  
  liftIO . putDoc $ indent 2 $ "compiled output written to" <+> pretty out_fname

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
