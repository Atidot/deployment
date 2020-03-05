{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Atidot.Deployment.Compile where

import           "data-default"  Data.Default (def)
import           "mtl"           Control.Monad.Reader  (asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "uniplate"      Data.Generics.Uniplate.Data (universeBi, childrenBi)
import           "text"          Data.Text (pack)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad

class ToResources a where
    toResources :: a -> Deploy Resources

instance (ToResources a) => ToResources (Use a) where
    toResources (No _)  = return []
    toResources (Yes x) = toResources x

instance ToResources VPC where
    toResources VPC{..}
        = return
        [ cluster
        ]
        where
            cluster :: Resource
            cluster = resource _vpc_name
                    $ ecsCluster
                    & ecscClusterName ?~ Literal _vpc_name

instance ToResources Cluster where
    toResources Cluster{..}
        = return
        $ [ cluster
          ]
       <> ( mconcat
          . map (runDeploy env . toResources)
          $ _cluster_tasks
          )
        where
            env :: Environment
            env = Environment
                 { _cluster = Just cluster
                 }

            cluster :: Resource
            cluster = resource _cluster_name ecsCluster


instance ToResources Registry where
    toResources Registry{..}
        = return
        [ ecr
        ]
        where
            ecr :: Resource
            ecr = resource (_registry_name)
                $ ecrRepository

instance ToResources CD where
    toResources _
        = return
        [ resource "CD" ecsCluster
        ]

instance ToResources Container where
    toResources Container{..}
        = do
            service' <- service
            return [ task
                   , service'
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

            service :: Deploy Resource
            service = do
                cluster' <- asks _cluster
                let clusterRef = (Ref . _resourceName) <$> cluster'
                return $ resource (_container_name <> "-Service")
                       $ ecsService
                       & ecssDesiredCount   ?~ Literal 1
                       & ecssServiceName    ?~ Literal _container_name
                       & ecssLaunchType     ?~ Literal "FARGATE"
                       & ecssTaskDefinition ?~ Ref (task ^. resourceName)
                       & ecssCluster        .~ clusterRef


toCloudFormation :: Deployment -> Template
toCloudFormation Deployment{..}
    = template rs
    & templateDescription   ?~ "Atidot ECS Deployment"
    & templateFormatVersion ?~ "2010-09-09"
    where
        rs :: Resources
        rs = mconcat
           $ [ runDeploy def . toResources $ _deployment_vpc
             , runDeploy def . toResources $ _deployment_cluster
             , runDeploy def . toResources $ _deployment_cd
             , runDeploy def . toResources $ _deployment_registry
             ]
