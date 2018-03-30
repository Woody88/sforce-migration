{-# LANGUAGE DeriveGeneric #-}

module Metadata.CustomObject where

import qualified Data.Text as T
import Data.Aeson
import Data.Yaml as Yaml
import Data.ByteString
import GHC.Generics
import Metadata.Commons
data CustomFieldType = Text | Date | DateTime | CheckBox | PickList deriving (Generic, Show)

newtype CustomObjectPackage = CustomObjectPackage { customObject :: CustomObject } deriving (Generic, Show)

data CustomObject = CustomObject
    { fullName                   :: FullName
    , plural                     :: T.Text 
    , label                      :: Label
    , deploymentStatus           :: DeploymentStatus
    , allowInChatterGroups       :: Maybe Bool
    , compactLayoutAssignment    :: Maybe T.Text
    , enableActivities           :: Maybe Bool
    , enableFeeds                :: Maybe Bool
    , enableHistory              :: Maybe Bool
    , enableReports              :: Maybe Bool
    , enableSearch               :: Maybe Bool
    , enableSharing              :: Maybe Bool
    , externalName               :: Maybe T.Text
    , externalRepository         :: Maybe T.Text
    , recordTypeTrackFeedHistory :: Maybe T.Text
    , recordTypeTrackHistory     :: Maybe T.Text
    , visibility                 :: Maybe Visibility
    } deriving (Generic, Show)

data CustomFields = CustomFields
    { fieldName  :: ByteString
    , fieldType  :: CustomFieldType
    , fieldLabel      :: Maybe ByteString
    } deriving (Generic, Show)


instance FromJSON CustomObjectPackage where
    parseJSON = genericParseJSON defaultOptions 

instance FromJSON CustomObject where 
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = customObjecField }


customObjecField "objectName" = "name"
customObjecField "objectFullName" = "fullName"
customObjecField f = f