{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Atidot.Deployment.Monad where

import "lens"         Control.Lens
import "base"         Data.Functor.Identity      (Identity, runIdentity)
import "data-default" Data.Default
import "mtl"          Control.Monad.Reader       (MonadReader)
import "transformers" Control.Monad.Trans.Writer (WriterT, runWriterT)
import "transformers" Control.Monad.Trans.Reader (ReaderT, runReaderT)
import "stratosphere" Stratosphere               (Resource)

newtype DeployT m d a
    = DeployT
    { unDeployT :: ReaderT d m a
    } deriving (Functor, Monad, MonadReader d, Applicative)


data Environment
    = Environment
    { _cluster :: !(Maybe Resource)
    }

makeLenses ''Environment

instance Default Environment where
    def = Environment Nothing

type Deploy a = DeployT Identity Environment a

runDeploy :: Environment
          -> Deploy a
          -> a
runDeploy env
    = runIdentity
    . flip runReaderT env
    . unDeployT
