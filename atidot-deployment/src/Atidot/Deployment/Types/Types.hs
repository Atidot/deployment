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
import "stratosphere"     Stratosphere (Resource)

data Use a
    = Yes a
    | No  a
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

data Deployment
    = Deployment
    { _deployment_region   :: !Region
    , _deployment_vpc      :: !(Use VPC)
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

data Region
    = US_EAST_2
    | US_EAST_1
    | US_WEST_1
    | US_WEST_2
    | AP_EAST_1
    | AP_SOUTH_1
    | AP_NORTHEAST_3
    | AP_NORTHEAST_2
    | AP_SOUTHEAST_1
    | AP_SOUTHEAST_2
    | AP_NORTHEAST_1
    | CA_CENTRAL_1
    | EU_CENTRAL_1
    | EU_WEST_1
    | EU_WEST_2
    | EU_WEST_3
    | EU_NORTH_1
    | ME_SOUTH_1
    | SA_EAST_1
    deriving (Eq, Ord, Data, Typeable, Generic)

instance Show Region where
   show US_EAST_2      = "us-east-2"
   show US_EAST_1      = "us-east-1"
   show US_WEST_1      = "us-west-1"
   show US_WEST_2      = "us-west-2"
   show AP_EAST_1      = "ap-east-1"
   show AP_SOUTH_1     = "ap-south-1"
   show AP_NORTHEAST_3 = "ap-northeast-3"
   show AP_NORTHEAST_2 = "ap-northeast-2"
   show AP_SOUTHEAST_1 = "ap-southeast-1"
   show AP_SOUTHEAST_2 = "ap-southeast-2"
   show AP_NORTHEAST_1 = "ap-northeast-1"
   show CA_CENTRAL_1   = "ca-central-1"
   show EU_CENTRAL_1   = "eu-central-1"
   show EU_WEST_1      = "eu-west-1"
   show EU_WEST_2      = "eu-west-2"
   show EU_WEST_3      = "eu-west-3"
   show EU_NORTH_1     = "eu-north-1"
   show ME_SOUTH_1     = "me-south-1"
   show SA_EAST_1      = "sa-east-1"

instance Read Region where
   readsPrec _ "us-east-2"      = [(US_EAST_2, "")]
   readsPrec _ "us-east-1"      = [(US_EAST_1, "")]
   readsPrec _ "us-west-1"      = [(US_WEST_1, "")]
   readsPrec _ "us-west-2"      = [(US_WEST_2, "")]
   readsPrec _ "ap-east-1"      = [(AP_EAST_1, "")]
   readsPrec _ "ap-south-1"     = [(AP_SOUTH_1, "")]
   readsPrec _ "ap-northeast-3" = [(AP_NORTHEAST_3, "")]
   readsPrec _ "ap-northeast-2" = [(AP_NORTHEAST_2, "")]
   readsPrec _ "ap-southeast-1" = [(AP_SOUTHEAST_1, "")]
   readsPrec _ "ap-southeast-2" = [(AP_SOUTHEAST_2, "")]
   readsPrec _ "ap-northeast-1" = [(AP_NORTHEAST_1, "")]
   readsPrec _ "ca-central-1"   = [(CA_CENTRAL_1, "")]
   readsPrec _ "eu-central-1"   = [(EU_CENTRAL_1, "")]
   readsPrec _ "eu-west-1"      = [(EU_WEST_1, "")]
   readsPrec _ "eu-west-2"      = [(EU_WEST_2, "")]
   readsPrec _ "eu-west-3"      = [(EU_WEST_3, "")]
   readsPrec _ "eu-north-1"     = [(EU_NORTH_1, "")]
   readsPrec _ "me-south-1"     = [(ME_SOUTH_1, "")]
   readsPrec _ "sa-east-1"      = [(SA_EAST_1, "")]

--
data Environment
    = Environment
    { _environment_registry :: !(Maybe Resource)
    , _environment_vpc      :: !(Maybe Resource)
    , _environment_cluster  :: !(Maybe Resource)
    , _environment_region   :: !(Maybe Region)
    }
    deriving (Show, Eq)


