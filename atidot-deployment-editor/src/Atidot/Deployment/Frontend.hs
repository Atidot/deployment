{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Atidot.Deployment.Frontend where

import           "base"                    Data.Maybe (isJust, fromJust)
import           "base"                    Data.Monoid (mconcat)
import           "base"                    Control.Monad.IO.Class (liftIO)
import           "base"                    Control.Applicative        ((<|>), Alternative)
import           "base"                    Data.Maybe (fromMaybe)
import           "base"                    Data.List (find)
import           "lens"                    Control.Lens hiding ((.=))
import           "data-default"            Data.Default (Default, def)
import           "aeson"                   Data.Aeson (Value(..), ToJSON, toJSON, fromJSON, Result(..), object, (.=))
import           "template-haskell"        Language.Haskell.TH.Syntax (Loc (..))
import           "vector"                  Data.Vector (toList)
import           "bytestring"              Data.ByteString.Lazy (toStrict, fromStrict)
import           "text"                    Data.Text (Text, unpack, pack)
import qualified "text"                    Data.Text as T (lines, splitOn, isInfixOf)
import           "text"                    Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           "cassava"                 Data.Csv (ToNamedRecord, FromNamedRecord, DefaultOrdered, encodeDefaultOrderedByName, decodeByName)
import           "stratosphere"            Stratosphere hiding ((.=))
import           "time"                    Data.Time.Clock (getCurrentTime)
import           "QuickCheck"              Test.QuickCheck.Arbitrary (Arbitrary)
import           "file-embed"              Data.FileEmbed
import           "reflex"                  Reflex
import           "reflex-dom"              Reflex.Dom hiding (Value, Error)

import           "reflex-utils"            Reflex.Utils
import           "reflex-mdl"              Reflex.MDL
import           "reflex-chartjs"          Reflex.ChartJS.ChartJS
import           "reflex-codemirror"       Reflex.CodeMirror
import           "reflex-jsoneditor"       Reflex.JsonEditor
import           "reflex-jsoneditor"       Reflex.JsonEditor.Types
import           "reflex-select2"          Reflex.Select2.Select2
import           "reflex-jexcel"           Reflex.JExcel
import           "reflex-fileapi"          Reflex.FileAPI.FileAPI
import           "atidot-deployment"       Atidot.Deployment.Types
import           "atidot-deployment"       Atidot.Deployment.Compile



defaultDeployment :: Deployment
defaultDeployment
    = def
    & deployment_cluster
        .~ ( Yes
           $ def
           & cluster_tasks
              .~ [ Yes $ def
                       & container_name .~ "undins"
                 , Yes $ def
                       & container_name .~ "jobrunner"
                 , Yes $ def
                       & container_name .~ "api"
                 , Yes $ def
                       & container_name .~ "log-server"
                 , Yes $ def
                       & container_name .~ "rabbitmq"
                 ]
           )


main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head'
            whenLoaded [headD] blank body
            return ()


--
head' :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head' = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.5.0/Chart.bundle.min.js"

                     , script "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/6.1.0/jsoneditor.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/6.1.0/jsoneditor.min.css"

                     , script "https://cdnjs.cloudflare.com/ajax/libs/material-design-lite/1.3.0/material.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/material-design-lite/1.3.0/material.min.css"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.css"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/theme/zenburn.css"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/arrive/2.4.1/arrive.min.js"
                     ]
    whenLoaded s1Ds blank $ do
        sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.7/js/select2.full.min.js"
                 , css    "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.7/css/select2.css.min"
                 , script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/mode/javascript/javascript.min.js"

                 , style
                 ]
        return ()

--
style :: forall t m. MonadWidget t m
      => m (Dynamic t Bool)
style = do
    _ <- el "style" $ text ".CodeMirror { border: 1px solid #eee; height: auto; }"
    return $ constDyn True

--
body :: forall t m. (MonadWidget t m)
     => m ()
body = mdlGrid $ do
    -- initial default configuration
    postBuildE <- getPostBuild
    let deploymentE = defaultDeployment <$ postBuildE
    (initialD :: Dynamic t Deployment) <- holdDyn def deploymentE

    -- json editor
    (userInputE :: Event t Deployment) <- mdlCell 4 $ do
        mdlGrid $ el "h3" $ text "Atidot Configuration"
        (xD, _) <- jsoneditor def initialD
        display xD
        return $ updated xD

    -- compile to CloudFormation
    let cloudformationE = toCloudFormation <$> userInputE
    let cfTextE = (decodeUtf8 . toStrict . encodeTemplate) <$> cloudformationE

    -- codemirror
    _ <- mdlCell 8 $ do
        mdlGrid $ el "h3" $ text "CloudFormation JSON"
        codemirror cmConfig cfTextE never

    return ()
    where
        cmConfig :: Configuration
        cmConfig
            = def
            & configuration_theme ?~ pack "zenburn"
            & configuration_mode ?~ object [ ("name" .= pack "javascript")
                                           , ("json" .= True)
                                           ]
