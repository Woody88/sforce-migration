{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Metadata.ActionOverride where

import Data.Data
import Data.Typeable
import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Metadata.Commons (strToLower)

data ActionName = Accept |  Clone | Delete | Edit | List | New | Tab | View deriving (Generic, Data, Typeable) 
data ActionType = Default | FlexiPage | LightningComponent | Scontrol | Standard | Visualforce deriving (Generic, Show)
data FormFactor = Large | Medium | Small deriving (Generic, Show)

data ActionOverride = ActionOverride
    { actionName           :: ActionName
    , actionType           :: ActionType
    , comment              :: Maybe Text 
    , content              :: Maybe Text
    , formFactor           :: Maybe FormFactor
    , skipRecordTypeSelect :: Maybe Bool
    } deriving (Generic, Show)

instance FromJSON ActionName
instance FromJSON ActionType
instance FromJSON FormFactor

instance FromJSON ActionOverride where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = actionOverrideField } 

actionOverrideField "actionType" = "type"
actionOverrideField f = f

instance Show ActionName where
    show = strToLower . showConstr . toConstr