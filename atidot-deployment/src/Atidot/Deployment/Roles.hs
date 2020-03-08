{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Atidot.Deployment.Roles where

import           "data-default"  Data.Default (def)
import           "mtl"           Control.Monad.Reader  (ask, asks)
import           "lens"          Data.Data.Lens hiding (template)
import           "aeson"         Data.Aeson
import           "text"          Data.Text (Text, pack)
import           "stratosphere"  Stratosphere hiding ((.=))
import                           Atidot.Deployment.Types
import                           Atidot.Deployment.Monad
import                           Atidot.Deployment.Compile


iam' :: Deploy Resources
iam' = do
    jenkins' <- resource "JenkinsRole" <$> jenkinsRole
    ec2'     <- resource "Ec2Role"     <$> ec2Role
    ecs'     <- resource "EcsRole"     <$> ecsRole
    profile' <- resource "Profile"     <$> profile ec2'
    return [ jenkins'
           , ec2'
           , ecs'
           , profile'
           ]
    where
        profile :: Resource -> Deploy IAMInstanceProfile
        profile ec2 = do
            return $ iamInstanceProfile [Literal "Profile"]
                   & iamipPath ?~ Literal "/"
                   & iamipRoles .~ [ ref ec2
                                   ]



        jenkinsRole :: Deploy IAMRole
        jenkinsRole = do
            return $ iamRole statement
                   & iamrPath ?~ Literal "/"
            where
                statement :: Object
                statement = [ "Statement" .= Array [ object
                                                   $ [ "Sid"       .= String ""
                                                     , "Effect"    .= String "Allow"
                                                     , "Principal" .= object ["Service" .= Array [String "ec2.amazonaws.com"]]
                                                     , "Action"    .= String "sts:AssumeRole"
                                                     ]
                                                   ]
                            ]

        ec2Role :: Deploy IAMRole
        ec2Role = do
            return $ iamRole statement
                   & iamrPath ?~ Literal "/"
                   & iamrPolicies ?~ [policy]
            where
                statement :: Object
                statement = [ "Statement" .= Array [ object
                                                   $ [ "Effect"    .= String "Allow"
                                                     , "Principal" .= object ["Service" .= Array [String "ec2.amazonaws.com"]]
                                                     , "Action"    .= Array [String "sts:AssumeRole"]
                                                     ]
                                                   ]
                            ]

                policy :: IAMRolePolicy
                policy = iamRolePolicy statement' "EC2Role"
                    where
                        statement' :: Object
                        statement' = [ "Statement" .= Array [ object
                                                            $ [ "Effect" .= String "Allow"
                                                              , "Action" .= Array [ String "ecs:CreateCluster",
                                                                                    String "ecs:RegisterContainerInstance",
                                                                                    String "ecs:DeregisterContainerInstance",
                                                                                    String "ecs:DiscoverPollEndpoint",
                                                                                    String "ecs:Submit*",
                                                                                    String "ecr:*",
                                                                                    String "ecs:Poll"
                                                                                  ]
                                                              , "Resource" .= String "*"
                                                              ]
                                                            ]
                                     ]

        ecsRole :: Deploy IAMRole
        ecsRole = do
            return $ iamRole statement
                   & iamrPath ?~ Literal "/"
                   & iamrPolicies ?~ [policy]
            where
                statement :: Object
                statement = [ "Statement" .= Array [ object
                                                   $ [ "Effect"    .= String "Allow"
                                                     , "Principal" .= object ["Service" .= Array [String "ec2.amazonaws.com"]]
                                                     , "Action"    .= Array [String "sts:AssumeRole"]
                                                     ]
                                                   ]
                            ]

                policy :: IAMRolePolicy
                policy = iamRolePolicy statement' "ECSRole"
                    where
                        statement' :: Object
                        statement' = [ "Statement" .= Array [ object
                                                            $ [ "Effect" .= String "Allow"
                                                              , "Action" .= Array [ String "elasticloadbalancing:Describe*"
                                                                                  , String "elasticloadbalancing:DeregisterInstancesFromLoadBalancer"
                                                                                  , String "elasticloadbalancing:RegisterInstancesWithLoadBalancer"
                                                                                  , String "ec2:Describe*"
                                                                                  , String "ec2:AuthorizeSecurityGroupIngress"
                                                                                  ]
                                                              , "Resource" .= String "*"
                                                              ]
                                                            ]
                                     ]



