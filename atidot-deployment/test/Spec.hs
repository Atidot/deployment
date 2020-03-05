{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import           "base"                  Control.Monad (mapM_)
import           "base"                  Data.List (group, sort)
import           "hspec"                 Test.Hspec
import           "QuickCheck"            Test.QuickCheck
import           "QuickCheck"            Test.QuickCheck.Monadic
import           "atidot-deployment"     Atidot.Deployment.Types

main :: IO ()
main = hspec $ do
    return ()
