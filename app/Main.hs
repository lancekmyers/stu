{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE 
    TypeApplications
  , MultiParamTypeClasses
  , PartialTypeSignatures
  , FlexibleInstances #-}

module Main where

import AST (Library, Program (Program), Elaboration, Parsing)
import Analysis
  ( Ctx,
    buildCtx,
    checkLib,
    checkModel,
    ctxFromSigs
  )
import Analysis.Error (TypeError)
import CodeGen (writeLib, writeProg)
import Control.Applicative (optional, (<**>))
import Data.Void (Void)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    MonadIO (liftIO),
    MonadTrans (lift),
    runExceptT,
    when,
    withExceptT,
  )
import Control.Monad.Reader (runReader)
import Control.Monad.State
  ( MonadIO (liftIO),
    MonadTrans (lift),
    StateT (runStateT),
    when,
  )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Options.Applicative
  ( Parser,
    argument,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    str,
    strOption,
    subparser
  )
import Parser (parseLibrary, parseProgram, parseSignatures)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    annotate,
    indent,
    line,
    (<+>),
  )
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, putDoc)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (-<.>))
import qualified Text.Builder as B
import Text.Megaparsec (SourcePos, errorBundlePretty, runParser)
import Types (Ty)
import Paths_stu
import Util (SrcSpan)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Control.Monad.Validate (validateToErrorWith, ValidateT)
import System.IO (hIsTerminalDevice, stdin)


data Options
  = BuildOptions
      { inFileName' :: FilePath,
        outFileName' :: Maybe FilePath
      }
  | CheckOptions
      {inFileName' :: FilePath}
  | LibOptions FilePath

options :: Parser Options
options =
  subparser $
    (command "build" (info buildOptions $ progDesc "Build a stu model"))
      <> (command "check" (info checkOptions $ progDesc "Check a stu model without building"))
      <> (command "lib" (info libOptions $ progDesc "Check a stu library and compile"))
  where
    buildOptions =
      BuildOptions
        <$> (argument str (metavar "MODEL" <> help "File containing model to compile"))
        <*> optional
          ( strOption
              ( long "output"
                  <> short 'o'
                  <> metavar "OUTPUT"
                  <> help "File to write python model to"
              )
          )
    checkOptions =
      CheckOptions
        <$> (argument str (metavar "MODEL" <> help "File containing model to compile"))

    libOptions =
      LibOptions
        <$> (argument str (metavar "LIBRARY" <> help "File containing librar"))


{-
   "         __       "
   "  _____/ /___  __ "
   " / ___/ __/ / / / "
   "(__  ) /_/ /_/ /  "
   "/____/\\__/\\__,_/"
-}

-- type Err = Report (Doc AnsiStyle)

parseFile :: FilePath -> ExceptT (Diagnostic Text) IO (Text, Program Parsing)
parseFile fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseProgram fname fcontents of
    Left bundle -> 
        let diag = errorDiagnosticFromBundle Nothing "Parse error" Nothing bundle 
        in throwError $ addFile diag fname (T.unpack fcontents) 
    Right prog -> return $ (fcontents, prog)

parseLibFile :: FilePath -> ExceptT (Diagnostic Text) IO (Text, Library Parsing)
parseLibFile fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseLibrary fname fcontents of
    Left bundle -> 
        let diag = errorDiagnosticFromBundle Nothing "Parse error" Nothing bundle 
        in throwError $ addFile diag fname (T.unpack fcontents) 
    Right lib -> return $ (fcontents, lib)

parseSig :: FilePath -> ExceptT (Diagnostic Text) IO Ctx
parseSig fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseSignatures fname fcontents of
    Left bundle -> 
        let diag = errorDiagnosticFromBundle Nothing "Parse error" Nothing bundle 
        in throwError $ addFile diag fname (T.unpack fcontents) 
    Right ctx -> return $ ctxFromSigs ctx

checkProgram
  :: Monad m 
  =>  Ctx -> Program Parsing -> ValidateT TypeError m (Program Elaboration, Ctx)
checkProgram ctx_std (Program decls model) = do
  let ctx = ctx_std <> buildCtx decls
  (model, ctx') <- runStateT (checkModel model) ctx
  return $ (Program decls model, ctx)

checkLibrary :: Monad m => Library Parsing -> ValidateT TypeError m (Library Elaboration, Ctx)
checkLibrary lib = do
  (lib', ctx') <- runStateT (checkLib lib) mempty
  return $ (lib', ctx')

validateFileNames 
  :: (FilePath, Maybe FilePath) 
  -> ExceptT (Diagnostic Text)  IO (FilePath, FilePath)
validateFileNames (inFileName, outFileName) 
  = withExceptT (addReport def) $ do
  let fname = inFileName
  let out_fname = fromMaybe (fname -<.> ".py") outFileName

  case takeExtension fname of
    ".stu" -> pure ()
    ext -> throwError $ Err Nothing
      ("Expected a '.stu' file, got: '" <> (T.pack ext) <> "'")
      [] []

  file_exists <- liftIO $ doesFileExist fname
  when (not file_exists) . throwError $ Err 
    Nothing
    "File does not exist"
    [] []

  return (fname, out_fname)

--handleTypeError 
--  :: FilePath -> ValidateT (Diagnostic Text) IO a
--  -> ExceptT (Diagnostic Text) IO a 
handleTypeError 
  :: (MonadIO m, MonadError (Diagnostic msg) m) 
  => FilePath -> ValidateT (Diagnostic msg) m b -> m b
handleTypeError fname x = do 
  contents <- liftIO $ readFile fname 
  let go diag = addFile diag fname contents
  validateToErrorWith go x

main' :: Options -> ExceptT (Diagnostic Text) IO ()
main' (BuildOptions inFileName outFileName) = do
  (fname, out_fname) <- validateFileNames (inFileName, outFileName)
  
  let handler = handleTypeError fname

  liftIO . putDoc $ "Checking  " <> (annotate bold . pretty $ fname) <> line

  (src, prog) <- parseFile fname
  std_lib_fname <- liftIO (getDataFileName "lib/std.stu")
  std_lib <- parseSig std_lib_fname
  (progChecked, ctx) <- handler $ checkProgram std_lib prog

  liftIO . putDoc $
    (annotate (color Green) $ "Success")
      <+> "checked"
      <+> pretty fname <> line

  let py_src = runReader (writeProg progChecked) ctx
  lift $ TIO.writeFile out_fname $ B.run py_src
  liftIO . putDoc $ indent 2 $ "compiled output written to" <+> pretty out_fname
main' (CheckOptions inFileName) = do
  (fname, _) <- validateFileNames (inFileName, Nothing)
  let handler = handleTypeError fname

  liftIO . putDoc $ "Checking  " <> (annotate bold . pretty $ fname) <> line

  (src, prog) <- parseFile fname
  std_lib_fname <- liftIO (getDataFileName "lib/std.stu")
  std_lib <- parseSig std_lib_fname
  progChecked <- handler $ checkProgram std_lib prog

  liftIO . putDoc $
    (annotate (color Green) $ "Success")
      <+> "checked"
      <+> pretty fname <> line
main' (LibOptions libFile) = do
  (fname, out_fname) <- validateFileNames (libFile, Nothing)
  let handler = handleTypeError fname
  (src, lib) <- parseLibFile fname
  (libChecked, ctx) <- handler $ checkLibrary lib
  -- let _ = traverse checkFunDef (_funs prog)
  let py_src = runReader (writeLib libChecked) mempty
  lift $ TIO.writeFile out_fname $ B.run py_src
  liftIO . putDoc $ indent 2 $ "compiled output written to" <+> pretty out_fname

instance HasHints Void msg where 
  hints _ = mempty
  {-
instance Pretty (Doc a) where 
  pretty = id
  -}
main :: IO ()
main = do 
  isTTY <- hIsTerminalDevice stdin
  given_options <- execParser opts
  runExceptT (main' given_options) >>= \case
    Left err -> error_printer isTTY err
    Right foo -> return ()
  where
    error_printer ispretty = printDiagnostic stdout ispretty ispretty 2 defaultStyle   
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Compile a stu model"
            <> header "stu"
        )

