{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           "base"                    Data.Maybe (fromJust)
import           "base"                    System.IO (stdin)
import           "text"                    Data.Text (Text)
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B (ByteString, readFile, putStrLn, hGetContents, unpack)
import qualified "aeson"                   Data.Aeson as JSON (eitherDecode, encode)
import qualified "aeson-pretty"            Data.Aeson.Encode.Pretty as JSON (encodePretty)
import           "stratosphere"            Stratosphere hiding ((.=))
import           "optparse-applicative"    Options.Applicative
import           "atidot-deployment"       Atidot.Deployment.Types
import           "atidot-deployment"       Atidot.Deployment.Deployment

data Example
    = Example
    deriving (Show)

data Translate
    = Translate
    deriving (Show)

data Command
    = C1 Example
    | C2 Translate
    deriving (Show)


exampleParser :: Parser Command
exampleParser = pure $ C1 Example

translateParser :: Parser Command
translateParser = pure $ C2 Translate

combined :: Parser Command
combined
    = subparser
    ( command "example" (info exampleParser (progDesc "Print exapmle Deployment"))
   <> command "translate" (info translateParser (progDesc "Translate Deployment JSON (<STDIN>) to CloudFormation JSON (<STDOUT>)"))
    )


onExample :: Example -> IO ()
onExample _ = do
    B.putStrLn . JSON.encodePretty $ exampleDeployment

onTranslate :: Translate -> IO ()
onTranslate _ = do
    eDeployment <- JSON.eitherDecode <$> B.hGetContents stdin
    case eDeployment of
        Left err -> error err
        Right deployment -> do
            let output = encodeTemplate
                       . toCloudFormation'
                       $ deployment
            B.putStrLn output


main :: IO ()
main = do
    options <- execParser (info (combined <**> helper)
                          (fullDesc <> progDesc "Atidot Deployment"))
    case options of
        C1 example   -> onExample example
        C2 translate -> onTranslate translate
