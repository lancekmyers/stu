{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module CodeGen.Util where 

import Control.Monad.Reader ( runReader, MonadReader, asks ) 
import Analysis (Ctx)
import Analysis.Context (Ctx(dists), lookupDistDefaultBij)
import qualified Data.Text as T

type CodeGenMonad m = (MonadReader Ctx m)