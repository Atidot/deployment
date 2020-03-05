{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Atidot.Deployment.Types.Default where

import "data-default" Data.Default
import "text"         Data.Text (Text)
import                Atidot.Deployment.Types.Types

instance (Default a) => Default (Use a) where
    def = Yes def

instance Default Deployment where
    def = Deployment
        { _deployment_vpc      = def
        , _deployment_registry = def
        , _deployment_cd       = def
        , _deployment_cluster  = def
        }

instance Default VPC where
    def = VPC "Atidot-VPC-Default"

instance Default Cluster where
    def = Cluster
        { _cluster_name  = "Atidot-Cluster-Default"
        , _cluster_tasks = []
        }

instance Default Registry where
    def = Registry
        { _registry_name  = "Atidot-Registry-Default"
        }

instance Default CD where
    def = CD
        { _cd_name = "Atidot-CD-Default"
        }

instance Default Container where
    def = Container
        { _container_name = "Atidot-Container-Default"
        , _container_cpu  = Just 2
        , _container_mem  = Just 8000
        , _container_disk = Just 4000
        }


