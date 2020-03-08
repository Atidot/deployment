{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Atidot.Deployment.Types.JSON where

import "base"                 Data.Typeable (Typeable, typeOf)
import "base"                 Data.Char (toLower)
import "lens"                 Control.Lens (over, _head)
import "data-default"         Data.Default (Default, def)
import "text"                 Data.Text (Text, pack)
import "aeson"                Data.Aeson
import "aeson"                Data.Aeson.TH
import "unordered-containers" Data.HashMap.Strict (elems, delete)
import "template-haskell"     Language.Haskell.TH
import                        Atidot.Deployment.Types.Types
import                        Atidot.Deployment.Types.Default

name :: (Typeable a) => a -> Text
name = pack . show . typeOf

instance (Typeable a, ToJSON a) => ToJSON (Use a) where
    toJSON (Yes x) = case toJSON x of
        (Object v) -> object
                    [ "use"    .= True
                    , (name x) .= toJSON x
                    ]

    toJSON (No x)  = case toJSON x of
        (Object v) -> object
                    [ "use"    .= False
                    , (name x) .= toJSON x
                    ]

instance (Default a, FromJSON a) => FromJSON (Use a) where
    parseJSON (Object v) = do
        use <- v .: "use"
        let v' = head . elems . delete "use" $ v
        original <- parseJSON v'
        if use
        then return $ Yes original
        else return $ No  original


--
instance ToJSON Deployment where
    toJSON Deployment{..}
        = object
        [ "region"   .= _deployment_region
        , "vpc"      .= _deployment_vpc
        , "registry" .= _deployment_registry
        , "cluster"  .= _deployment_cluster
        , "cd"       .= _deployment_cd
        ]

instance FromJSON Deployment where
    parseJSON (Object v) = do
        Deployment <$>  v .:  "region"
                   <*> (v .:? "vpc"      .!= def)
                   <*> (v .:? "registry" .!= def)
                   <*> (v .:? "cluster"  .!= def)
                   <*> (v .:? "cd"       .!= def)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length ("_vpc_" :: String))} ''VPC)
$(deriveJSON defaultOptions{fieldLabelModifier = drop (length ("_cluster_" :: String))} ''Cluster)
$(deriveJSON defaultOptions{fieldLabelModifier = drop (length ("_registry_" :: String))} ''Registry)
$(deriveJSON defaultOptions{fieldLabelModifier = drop (length ("_cd_" :: String))} ''CD)
$(deriveJSON defaultOptions{fieldLabelModifier = drop (length ("_container_" :: String))} ''Container)
$(deriveJSON defaultOptions{constructorTagModifier = map (\c -> if c == '_' then '-' else c) . map toLower} ''Region)
-- $(deriveJSON defaultOptions{fieldLabelModifier = drop (length ("_deployment_" :: String))} ''Deployment)
