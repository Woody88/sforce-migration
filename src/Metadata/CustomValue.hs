{-# LANGUAGE DeriveGeneric #-}
module Metadata.CustomValue where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Metadata.Commons (Label)

data CustomValue = CustomValue
    { label               :: Label
    , customValudeDefault :: Bool
    , color               :: Maybe Text 
    , description         :: Maybe Text
    , isActive            :: Maybe Bool
    } deriving (Generic, Show)


instance FromJSON CustomValue where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = customValueField }
    
customValueField "customValudeDefault" = "default"
customValueField x = x

