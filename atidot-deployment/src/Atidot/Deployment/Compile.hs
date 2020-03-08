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
import           "mtl"           Control.Monad.Reader  (ask, asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "aeson"         Data.Aeson
import           "text"          Data.Text (Text, pack, unpack)
import           "casing"        Text.Casing (fromAny, toPascal)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad


ref :: Resource -> Val Text
ref r = Ref (r ^. resourceName)


envDependency :: (Environment -> Maybe Resource) -> Deploy (Maybe [Text])
envDependency accessor = do
    mResource <- asks accessor
    return $ do
        -- Maybe monad
        resource' <- mResource
        return [resource' ^. resourceName]


camelify :: Text -> Text
camelify
    = pack
    . toPascal
    . fromAny
    . unpack

--
class ToResources a where
    toResources :: a -> Deploy Resources


--
instance (ToResources a) => ToResources (Use a) where
    toResources (No _)  = return []
    toResources (Yes x) = toResources x
