{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Atidot.Deployment.ECS where

import           "stratosphere" Stratosphere

ecs :: Resource
ecs
    = resource "ECSCluster" cluster
    where
        cluster :: ECSCluster
        cluster = ecsCluster
                & ecscClusterName ?~ "ECSCluster"



