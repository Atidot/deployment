{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Atidot.Deployment.Container where

import           "data-default"  Data.Default (def)
import           "mtl"           Control.Monad.Reader  (ask, asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "aeson"         Data.Aeson
import           "text"          Data.Text (Text, pack)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad
import                           Atidot.Deployment.Compile

instance ToResources Container where
    toResources Container{..} = do

        -- task
        (task :: Resource) <- task'

        -- service
        (service :: Resource) <- service' task

        -- result
        (deps :: Maybe [Text]) <- envDependency _environment_vpc
        let (rs :: Resources) = [ task & resourceDependsOn    .~ deps
                                , service & resourceDependsOn .~ deps
                                ]
        return rs
        where
            service' :: Resource -> Deploy Resource
            service' task = do
                mClusterRef <- (fmap ref) <$> asks _environment_cluster
                return $ resource (camelify _container_name <> "Service")
                       $ ecsService
                       & ecssDesiredCount   ?~ Literal 1
                       & ecssServiceName    ?~ Literal _container_name
                       & ecssLaunchType     ?~ Literal "EC2"
                       & ecssTaskDefinition ?~ ref task
                       & ecssCluster        .~ mClusterRef

            task' :: Deploy Resource
            task' = do
                mContainer <- container'
                let containers = case mContainer of
                                    Nothing -> []
                                    Just container -> [container]

                return $ resource (camelify _container_name <> "Task")
                       $ ecsTaskDefinition
                       & ecstdContainerDefinitions ?~ containers
                       -- & ecstdCpu    .~ (Literal <$> _container_cpu)
                       -- & ecstdMemory .~ (Literal <$> _container_mem)

            container' :: Deploy (Maybe ECSTaskDefinitionContainerDefinition)
            container' = do
                mImage <- image'
                return $ do
                    -- Maybe Monad
                    image <- mImage
                    return $ ecsTaskDefinitionContainerDefinition (Literal _container_name)
                                                                  (Literal _container_name)
                           & ecstdcdCpu    .~ (Literal <$> _container_cpu)
                           & ecstdcdMemory .~ (Literal <$> _container_mem)
                           & ecstdcdImage  .~ image

            image' :: Deploy (Maybe (Val Text))
            image' = do
                mRegion  <- asks _environment_region
                mEcrRepo <- asks _environment_registry
                return $ do
                    -- Maybe Monad
                    region  <- (pack . show) <$> mRegion
                    ecrRepo <- mEcrRepo
                    return $ Join ""
                           [ Ref "AWS::AccountId"
                           , Literal $ ".dkr.ecr." <> region <> ".amazonaws.com/"
                           , ref ecrRepo
                           , ":"
                           , Literal "latest"
                           ]
