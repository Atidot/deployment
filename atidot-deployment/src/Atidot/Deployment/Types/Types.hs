{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Atidot.Deployment.Types.Types where

import "base"             GHC.Generics (Generic)
import "base"             Data.Monoid (Monoid(..))
import "base"             Data.Data (Data)
import "base"             Data.Typeable (Typeable)
import "text"             Data.Text (Text)
import "aeson"            Data.Aeson

data Use a
    = Yes a
    | No  a
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Deployment
    = Deployment
    { _deployment_vpc      :: !(Use VPC)
    , _deployment_registry :: !(Use Registry)
    , _deployment_cluster  :: !(Use Cluster)
    , _deployment_cd       :: !(Use CD)
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data VPC
    = VPC
    { _vpc_name :: !Text
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data CD
    = CD
    { _cd_name :: !Text
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Cluster
    = Cluster
    { _cluster_name  :: !Text
    , _cluster_tasks :: !([Use Container])
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Registry
    = Registry
    { _registry_name :: !Text
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Container
    = Container
    { _container_name :: !Text
    , _container_cpu  :: !(Maybe Integer)
    , _container_mem  :: !(Maybe Integer)
    , _container_disk :: !(Maybe Integer)
    }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)
