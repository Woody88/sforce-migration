{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- Common types referring the developer saleforce page https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/meta_field_types.htm#meta_type_deploy_stat_type -}

module Metadata.Commons where

import Data.Char
import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Text.XML.HaXml.XmlContent hiding (String)
import Text.XML.HaXml.Util 
import Text.XML.HaXml.Types

newtype Label = Label Text deriving (Generic, Show)
newtype FullName = FullName Text deriving (Generic, Show)
    
data DeploymentStatus   = Deployed | InDevelopment deriving (Generic, Show)
data DeleteConstraint   = SetNull | Restrict | Cascade  deriving (Generic, Show)
data Visibility         = Public | Protected deriving (Generic, Show)
data Gender             = Masculine | Feminine | Neuter | AnimateMasculine deriving (Generic, Show)
data SharingModel       = Private | Read | ReadWrite | ReadWriteTransfer | FullAccess | ControlledByParent deriving (Generic, Show)
data StartsWith         = Consonant | Vowel | Special deriving (Generic, Show)
data TreatBlanksAs      = BlankAsBlank | BlankAsZero deriving (Generic, Show)
data FieldManageability =  Locked | DeveloperControlled | SubscriberControlled deriving (Generic, Show)

data FieldType 
    = AutoNumber | Lookup | MasterDetail | MetadataRelationship | Checkbox | Currency | Date | DateTime | Email | EncryptedText
    | ExternalLookup | IndirectLookup | Number1 | Percent | Phone | Picklist | MultiselectPicklist | Summary | Text | TextArea 
    | LongTextArea | Url | Hierarchy | File | CustomDataType | Html | Location  | Time 
    deriving (Generic, Show)


data PickList = PickList
    { controllingField   :: Text 
    , picklistValues     :: [PickListValues] 
    , restrictedPicklist :: Maybe Bool
    , sorted             :: Maybe Bool
    } deriving (Generic, Show)

data PickListValues = PickListValues 
    { fullName :: FullName 
    , controllingFieldValues :: Maybe Bool
    , pickListValueDefault   :: Maybe Bool
    } deriving (Generic, Show)

class MetaPackage a where
    unwrap :: a -> b

instance FromJSON FullName 
instance FromJSON Label 
instance FromJSON DeploymentStatus
instance FromJSON DeleteConstraint
instance FromJSON Visibility
instance FromJSON Gender
instance FromJSON SharingModel
instance FromJSON StartsWith
instance FromJSON TreatBlanksAs
instance FromJSON FieldManageability
instance FromJSON FieldType
instance FromJSON PickList

instance FromJSON PickListValues where 
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = pickListValuesField }

pickListValuesField "pickListValueDefault" = "default"

strToLower :: Show a => a -> String
strToLower = toLowerCase . show 

toLowerCase :: String -> String
toLowerCase []     = []
toLowerCase (x:xs) = toLower x : toLowerCase xs

toUpperCase :: String -> String
toUpperCase []     = []
toUpperCase (x:xs) = toUpper x : toUpperCase xs 

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs 

mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

optionalContent :: HTypeable a => String -> Maybe a -> [Content ()]
optionalContent _ Nothing = []
optionalContent el (Just c) = toContents' el c
    where toContents' e c = [mkAttrElemC e [] (toText . value $ toHType c)]
          value (Defined v _ _) = v
          value (Prim _ v)      = v