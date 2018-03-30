{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Metadata.ValueSet where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Metadata.CustomValue

data ValueSet = ValueSet 
    { controllingField :: Text 
    , restricted       :: Maybe Bool
    } deriving (Generic, Show)

data ValueSetValuesDefinition = ValueSetValuesDefinition
    { value  :: CustomValue 
    , sorted :: Maybe Bool
    } deriving (Generic, Show)


data ValueSettings = ValueSettings
    { controllingFieldValue :: [Text]
    , valueName             :: Text 
    } deriving (Generic, Show)

instance FromJSON ValueSet
instance FromJSON ValueSetValuesDefinition
instance FromJSON ValueSettings