{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Atidot.Deployment.Monad where

import "base"         Data.Functor.Identity      (Identity, runIdentity)
import "text"         Data.Text                  (Text)
import "mtl"          Control.Monad.Reader       (MonadReader)
import "transformers" Control.Monad.Trans.Writer (WriterT, runWriterT)
import "transformers" Control.Monad.Trans.Reader (ReaderT, runReaderT)
import "stratosphere" Stratosphere               (Resource)
import                Atidot.Deployment.Types

newtype DeployT m d a
    = DeployT
    { unDeployT :: ReaderT d m a
    } deriving (Functor, Monad, MonadReader d, Applicative)

type Deploy a = DeployT Identity Environment a

runDeploy :: Environment
          -> Deploy a
          -> a
runDeploy env
    = runIdentity
    . flip runReaderT env
    . unDeployT
