{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           "base"                    Data.Functor.Identity (Identity)
import           "text"                    Data.Text (Text)
import           "base"                    System.IO (stdin)
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B (ByteString, readFile, putStrLn, hGetContents, unpack)
import           "vector"                  Data.Vector (Vector)
import qualified "cassava"                 Data.Csv as CSV (Header, encodeDefaultOrderedByName, decodeByName)
import qualified "aeson"                   Data.Aeson as JSON (encode)
import           "QuickCheck"              Test.QuickCheck.Arbitrary
import           "QuickCheck"              Test.QuickCheck.Gen
import           "optparse-applicative"    Options.Applicative
import           "atidot-deployment"       Atidot.Deployment.Types

main :: IO ()
main = do
    return ()
