{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module CodeGen.Util where 

import Control.Monad.Reader ( MonadReader ) 
import Analysis.Context ( Ctx )

type CodeGenMonad m = (MonadReader Ctx m)