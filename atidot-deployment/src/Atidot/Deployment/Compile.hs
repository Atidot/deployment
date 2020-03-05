{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Atidot.Deployment.Compile where

import           "text"          Data.Text (pack)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types

class ToResources a where
    toResources :: a -> Resources

instance (ToResources a) => ToResources (Use a) where
    toResources (No _)  = []
    toResources (Yes x) = toResources x

instance ToResources VPC where
    toResources VPC{..}
        = [ cluster
          ]
        where
            cluster :: Resource
            cluster = resource _vpc_name
                    $ ecsCluster
                    & ecscClusterName ?~ Literal _vpc_name

instance ToResources Cluster where
    toResources Cluster{..}
        = [ cluster
          ]
       <> (mconcat . map toResources $ _cluster_tasks)
        where
            cluster :: Resource
            cluster = resource _cluster_name ecsCluster

instance ToResources Registry where
    toResources Registry{..}
        = [ ecr
          ]
        where
            ecr :: Resource
            ecr = resource (_registry_name)
                $ ecrRepository

instance ToResources CD where
    toResources _ = [resource "CD" ecsCluster]

instance ToResources Container where
    toResources Container{..}
        = [ task
          , service
          ]
        where
            container = ecsTaskDefinitionContainerDefinition (Literal _container_name)
                                                             (Literal _container_name)
                      & ecstdcdCpu    .~ (Literal <$> _container_cpu)
                      & ecstdcdMemory .~ (Literal <$> _container_mem)

            task :: Resource
            task = resource (_container_name <> "-Task")
                 $ ecsTaskDefinition
                 & ecstdContainerDefinitions ?~ [container]
                 & ecstdCpu    .~ (Literal . pack . show <$> _container_cpu)
                 & ecstdMemory .~ (Literal . pack . show <$> _container_mem)

            service :: Resource
            service = resource (_container_name <> "-Service")
                    $ ecsService
                    & ecssServiceName ?~ Literal _container_name
                    & ecssLaunchType ?~ Literal "FARGATE"
                    & ecssTaskDefinition ?~ Ref (task ^. resourceName)


toCloudFormation :: Deployment -> Template
toCloudFormation Deployment{..}
    = template rs
    & templateDescription   ?~ "Atidot ECS Deployment"
    & templateFormatVersion ?~ "2010-09-09"
    where
        rs :: Resources
        rs = mconcat
           [ toResources _deployment_vpc
           , toResources _deployment_cluster
           , toResources _deployment_cd
           , toResources _deployment_registry
           ]
