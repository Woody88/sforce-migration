{-# LANGUAGE DeriveGeneric #-}
module Metadata.LookupFilter where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data LookupFilter = LookupFilter
    { active        :: Bool
    , isOptional    :: Bool
    , filterItems   :: FilterItem
    , booleanFilter :: Maybe Text
    , description   :: Maybe Text
    , errorMessage  :: Maybe Text 
    , infoMessage   :: Maybe Text 
    } deriving (Generic, Show)

data FilterItem = FilterItem
    { field      :: Text
    , operation  :: FilterOperation
    , value      :: Text 
    , valueField :: Maybe Text
    } deriving (Generic, Show)

data FilterOperation 
    = Equals | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual 
    | Contains | NotContain | StartsWith | Includes | Excludes | Within 
    deriving (Generic, Show)


instance FromJSON LookupFilter
instance FromJSON FilterItem
instance FromJSON FilterOperation