{-# LANGUAGE DeriveGeneric #-}
module Metadata.CustomField where

import Data.Text         (Text)  
import Data.Aeson
import GHC.Generics
import Metadata.ValueSet (ValueSet)
import Metadata.LookupFilter   (LookupFilter, FilterItem)
import Metadata.Commons  (FullName, Label, FieldType, DeleteConstraint, FieldManageability, TreatBlanksAs, PickList)

data SummaryOperation
    = Count | Min | Max | Sum
    deriving (Generic, Show)

data CustomField = CustomField 
    { fullName                             :: FullName 
    , label                                :: Label
    , fieldType                            :: FieldType
    , description                          :: Maybe Text 
    , caseSensitive                        :: Maybe Text
    , customDataType                       :: Maybe Text
    , defaultValue                         :: Maybe Text
    , deleteConstraint                     :: Maybe DeleteConstraint
    , deprecated                           :: Maybe Bool
    , displayFormat                        :: Maybe Text
    , displayLocationInDecimal             :: Maybe Bool
    , encrypted                            :: Maybe Bool
    , externalDeveloperName                :: Maybe Text 
    , externalId                           :: Maybe Bool
    , fieldManageability                   :: Maybe FieldManageability
    , formula                              :: Maybe Text
    , formulaTreatBlankAs                  :: Maybe TreatBlanksAs
    , globalPicklist                       :: Maybe Text
    , indexed                              :: Maybe Bool
    , inlineHelpText                       :: Maybe Text
    , isFilteringDisabled                  :: Maybe Bool
    , isNameField                          :: Maybe Bool
    , isSortingDisabled                    :: Maybe Bool
    , reparentableMasterDetail             :: Maybe Bool
    , length                               :: Maybe Int
    , lookupFilter                         :: Maybe LookupFilter
    , metadataRelationshipControllingField :: Maybe Bool
    , picklist                             :: Maybe PickList
    , populateExistingRows                 :: Maybe Bool
    , precision                            :: Maybe Int
    , referenceTargetField                 :: Maybe Text
    , referenceTo                          :: Maybe Text
    , relationshipLabel                    :: Maybe Text
    , relationshipName                     :: Maybe Text
    , relationshipOrder                    :: Maybe Int 
    , required                             :: Maybe Bool
    , scale                                :: Maybe Int
    , startingNumber                       :: Maybe Int
    , stripMarkup                          :: Maybe Bool
    , summarizedField                      :: Maybe Text
    , summaryFilterItems                   :: Maybe FilterItem
    , summaryForeignKey                    :: Maybe Text
    , summaryOperation                     :: Maybe SummaryOperation
    , trackFeedHistory                     :: Maybe Bool
    , trackHistory                         :: Maybe Bool
    , trackTrending                        :: Maybe Bool
    , trueValueIndexed                     :: Maybe Bool
    , unique                               :: Maybe Bool
    , valueSet                             :: Maybe ValueSet
    , visibleLines                         :: Maybe Int
    , writeRequiresMasterRead              :: Maybe Bool
    } deriving (Generic, Show)

    
instance FromJSON SummaryOperation
instance FromJSON CustomField where 
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = customFieldP }


customFieldP "fieldType" = "type"  
customFieldP f = f
