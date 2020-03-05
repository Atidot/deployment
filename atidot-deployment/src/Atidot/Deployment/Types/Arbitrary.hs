{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Atidot.Deployment.Types.Arbitrary where

import "base" Control.Monad.IO.Class (liftIO)
import "base"         Data.Data
import "QuickCheck"   Test.QuickCheck.Arbitrary
import "QuickCheck"   Test.QuickCheck.Gen
import "data-default" Data.Default (def)
import "text"         Data.Text (Text, pack)
import                Atidot.Deployment.Types.Types
