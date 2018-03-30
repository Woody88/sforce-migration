{-# LANGUAGE DeriveGeneric #-}
module Metadata.Commons where

import Data.Char
import Data.Text (Text)
import Data.Aeson
import GHC.Generics

newtype Label = Label Text deriving (Generic, Show)
newtype FullName = FullName Text deriving (Generic, Show)

data DeploymentStatus = Deployed | InDevelopment deriving (Generic, Show)
data Visibility = Public | Protected deriving (Generic, Show)

instance FromJSON FullName 
instance FromJSON Label 
instance FromJSON DeploymentStatus
instance FromJSON Visibility

strToLower :: Show a => a -> String
strToLower = toLowerCase . show 

toLowerCase :: String -> String
toLowerCase []     = []
toLowerCase (x:xs) = toLower x : toLowerCase xs