{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Atidot.Deployment.Deployment where

import           "data-default"  Data.Default (def)
import           "mtl"           Control.Monad.Reader  (ask, asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "aeson"         Data.Aeson
import           "text"          Data.Text (Text, pack)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad
import                           Atidot.Deployment.Compile
import                           Atidot.Deployment.VPC
import                           Atidot.Deployment.CD
import                           Atidot.Deployment.Registry
import                           Atidot.Deployment.Roles
import                           Atidot.Deployment.Cluster
import                           Atidot.Deployment.Container



exampleDeployment :: Deployment
exampleDeployment
    = def
    & deployment_region .~ US_EAST_2
    & deployment_cluster
        .~ ( Yes
           $ def
           & cluster_tasks
              .~ [ Yes $ def
                       & container_name .~ "undins"
                 , Yes $ def
                       & container_name .~ "jobrunner"
                 , Yes $ def
                       & container_name .~ "api"
                 , Yes $ def
                       & container_name .~ "log-server"
                 , Yes $ def
                       & container_name .~ "rabbitmq"
                 ]
           )


--
toCloudFormation :: Deployment -> Deploy Template
toCloudFormation Deployment{..} = do
    vpc <- toResources _deployment_vpc
    cd  <- toResources _deployment_cd
    ecr <- toResources _deployment_registry
    iam <- iam'
    env <- ask
    let env' = env
             & environment_vpc      ?~ (head . unResources $ vpc)
             & environment_registry ?~ (head . unResources $ ecr)
    --deps <- runDeploy env' . dependencies $ _environment_vpc
    let cluster = runDeploy env' . toResources $ _deployment_cluster
    let rs = mconcat
           [ vpc
           , cd
           , ecr
           , iam
           , cluster -- & resourceDependsOn {- ?~ [ vpc ^. resourceName-}
                                            {-]-}
           ]
    return $ template rs
           & templateDescription   ?~ "Atidot ECS Deployment"
           & templateFormatVersion ?~ "2010-09-09"

toCloudFormation' :: Deployment -> Template
toCloudFormation' deployment
    = runDeploy env . toCloudFormation $ deployment
    where
        env :: Environment
        env = def
            & environment_region ?~ (_deployment_region deployment)
