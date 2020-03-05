{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Atidot.Deployment.Types.Lens where

import "lens" Control.Lens
import        Atidot.Deployment.Types.Types

makeClassy ''Deployment
makeClassy ''VPC
makeClassy ''Registry
makeClassy ''CD
makeClassy ''Cluster
makeClassy ''Container
