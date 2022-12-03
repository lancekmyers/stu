{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeGen.Util where

import Analysis.Context (Ctx)
import Control.Monad.Reader (MonadReader)

type CodeGenMonad m = (MonadReader Ctx m)
