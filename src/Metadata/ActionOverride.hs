{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-| Following Developer Salesforce Criterias for ActionOverride https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/actionoverride.htm#actionoverride |-}
module Metadata.ActionOverride where

import Data.Data
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Control.Monad

data ActionName = Accept |  Clone | Delete | Edit | List | New | Tab | View deriving (Generic, Read, Show, Data, Typeable) 
data ActionType = Default | Flexipage | Lightningcomponent | Scontrol | Standard | Visualforce deriving (Generic, Read, Show, Data, Typeable) 
data FormFactor = Large | Medium | Small deriving (Generic, Show)

data ActionOverride = ActionOverride
    { actionName           :: ActionName
    , actionType           :: ActionType
    , comment              :: Maybe Text 
    , content              :: Maybe Text
    , formFactor           :: Maybe FormFactor
    , skipRecordTypeSelect :: Maybe Bool
    } deriving (Generic, Show)

instance FromJSON ActionName where
    parseJSON (String s) = fmap read (pure $ T.unpack . T.toTitle $ s)
    parseJSON _ = mzero

instance FromJSON ActionType where
    parseJSON (String s) = fmap read (pure $ T.unpack . T.toTitle $ s)
    parseJSON _ = mzero

instance FromJSON FormFactor

instance FromJSON ActionOverride where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = actionOverrideField } 

actionOverrideField "actionType" = "type"
actionOverrideField f = f 



