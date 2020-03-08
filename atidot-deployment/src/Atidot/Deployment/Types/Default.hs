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
        { _deployment_region   = def
        , _deployment_vpc      = def
        , _deployment_registry = def
        , _deployment_cd       = def
        , _deployment_cluster  = def
        }

instance Default VPC where
    def = VPC "VPC"

instance Default Cluster where
    def = Cluster
        { _cluster_name  = "Cluster"
        , _cluster_tasks = []
        }

instance Default Registry where
    def = Registry
        { _registry_name  = "Registry"
        }

instance Default CD where
    def = CD
        { _cd_name = "CD"
        }

instance Default Container where
    def = Container
        { _container_name = "Container"
        , _container_cpu  = Just 2
        , _container_mem  = Just 8000
        , _container_disk = Just 4000
        }

instance Default Region where
    def = US_EAST_2

instance Default Environment where
    def = Environment Nothing
                      Nothing
                      Nothing
                      Nothing




