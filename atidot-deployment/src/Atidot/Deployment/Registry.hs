{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Atidot.Deployment.Registry where

import           "data-default"  Data.Default (def)
import           "mtl"           Control.Monad.Reader  (ask, asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "aeson"         Data.Aeson
import           "text"          Data.Text (Text, pack, toLower)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad
import                           Atidot.Deployment.Compile

instance ToResources Registry where
    toResources Registry{..} = do
        -- ecr
        let (ecr :: ECRRepository) = ecrRepository
                                   & ecrrRepositoryName ?~ Literal (toLower _registry_name)
                                   & ecrrRepositoryPolicyText ?~ policyText

        -- resource
        let (resource' :: Resource) = resource _registry_name ecr

        -- result
        let (rs :: Resources) = [resource']
        return rs

        where
            policyText :: Object
            policyText
                = [ "Version" .= String "2008-10-17"
                  , "Statement" .= Array [policy]
                  ]

            policy :: Value
            policy
                = object [ "Sid"    .= String "AllowPushPull"
                , "Effect" .= String "Allow"
                , "Principal" .= object
                               [ "AWS" .= Array [iam]
                               ]
                , "Action" .= Array
                            [ String "ecr:GetDownloadUrlForLayer"
                            , String "ecr:BatchGetImage"
                            , String "ecr:BatchCheckLayerAvailability"
                            , String "ecr:PutImage"
                            , String "ecr:InitiateLayerUpload"
                            , String "ecr:UploadLayerPart"
                            , String "ecr:CompleteLayerUpload"
                            ]
                ]

            iam :: Value
            iam = toJSON
                $ Join ""
                [ Literal "arn:aws:iam::"
                , Ref "AWS::AccountId"
                , Literal ":user/"
                , Literal "atidot"
                ]
