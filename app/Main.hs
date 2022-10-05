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
import Data.Maybe (mapMaybe)
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

clearScreen :: IO ()
clearScreen = do
  putStr clearFromCursorToScreenBeginningCode
  putStr $ setCursorPositionCode 0 0
  putStrLn "         __       "
  putStrLn "  _____/ /___  __ "
  putStrLn " / ___/ __/ / / / "
  putStrLn "(__  ) /_/ /_/ /  "
  putStrLn "/____/\\__/\\__,_/"
  putStrLn ""

type Err = Doc ()

{-
getFileName :: ExceptT Err IO String
getFileName = do
  args <- lift getArgs
  case args of
    [] -> throwError "No file name provided"
    [fname] -> return fname
    _  -> throwError "To many files provided"
-}
parseFile :: String -> ExceptT Err IO ([Decl], Model SourcePos)
parseFile fname = do
  fcontents <- lift $ TIO.readFile fname
  case runParser parseProgram fname fcontents of
    Left err -> throwError . pretty $ errorBundlePretty err
    Right prog -> return prog

main' :: FilePath -> ExceptT Err IO ()
main' fname = do
  prog@(decls, model) <- parseFile fname
  let ctx = buildCtx decls
  (model', _) <- withExceptT pretty $ runStateT (checkModel model) ctx
  let py_src = B.run $ writeProg (Program decls model') 
    -- withExceptT pretty $ evalStateT (cgProg prog) ctx
  lift $ TIO.writeFile (fname -<.> ".py") py_src
  return ()

-- putStrLn "Done!"

action :: Event -> IO ()
action (Modified path _ _) = do
  putStrLn path
  x <- runExceptT (main' path)
  case x of
    Left err -> clearScreen >> putDoc err >> putStrLn ""
    Right () -> do
      clearScreen
      putStrLn $ "Successfully compiled " ++ path
action _ = pure ()

main :: IO ()
main = do
  putStrLn "Welcome!"
  fname <-
    getArgs >>= \case
      [fname] -> return fname
      [] -> error "No file name provided"
      _ -> error "To many files provided"

  case takeExtension fname of
    ".stu" -> pure ()
    ext -> error $ "Expected a '.stu' file, got: '" ++ ext ++ "'"

  fname' <- canonicalizePath fname

  file_exists <- doesFileExist fname'
  when (not file_exists) (error "Given file does not exist.")

  runExceptT (main' fname') >>= \case 
    Left err -> print err
    Right foo -> return ()

  {-
  let dir = takeDirectory fname
  let pred = \event -> takeExtension (eventPath event) == ".stu"
  -- let pred =  \event -> (eventPath event) `equalFilePath` fname
  
  withManager $ \mgr -> do
    watchDir
      mgr
      dir
      pred
      action

    forever $ threadDelay 1000000
  -}

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
