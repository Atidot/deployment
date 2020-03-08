{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Atidot.Deployment.VPC where

import           "data-default"  Data.Default (def)
import           "mtl"           Control.Monad.Reader  (ask, asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "aeson"         Data.Aeson
import           "text"          Data.Text (Text, pack)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad
import                           Atidot.Deployment.Compile

instance ToResources VPC where
    toResources VPC{..} = do
        -- vpc
        let (vpc :: EC2VPC) = ec2VPC (Literal "10.0.0.0/24")

        -- resource
        let (resource' :: Resource) = resource _vpc_name vpc

        -- result
        let (rs :: Resources) = [resource']
        return rs
