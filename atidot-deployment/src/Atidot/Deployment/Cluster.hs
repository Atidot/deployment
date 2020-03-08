{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Atidot.Deployment.Cluster where

import           "data-default"  Data.Default (def)
import           "mtl"           Control.Monad.Reader  (ask, asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "aeson"         Data.Aeson
import           "text"          Data.Text (Text, pack)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad
import                           Atidot.Deployment.Compile
import                           Atidot.Deployment.Container

instance ToResources Cluster where
    toResources Cluster{..} = do
        -- cluster
        let (cluster :: ECSCluster) = ecsCluster

        -- resource
        (deps :: Maybe [Text]) <- envDependency _environment_vpc
        let (resource' :: Resource) = resource _cluster_name cluster
                                    & resourceDependsOn .~ deps

        -- tasks
        env' <- ask
        let env = env'
                & environment_cluster ?~ resource'
        let (tasks :: Resources) = mconcat
                                 . map (runDeploy env . toResources)
                                 $ _cluster_tasks

        -- result
        let (rs :: Resources) = [resource']
                             <> tasks
        return rs
