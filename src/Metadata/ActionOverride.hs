{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- Following Developer Salesforce Criterias for ActionOverride https://developer.salesforce.com/docs/atlas.en-us.api_meta.meta/api_meta/actionoverride.htm#actionoverride -}
module Metadata.ActionOverride where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Control.Monad
import Text.XML.HaXml.XmlContent hiding (String)
import Text.XML.HaXml.Util 
import Text.XML.HaXml.Types
import Metadata.Commons (strToLower, capitalize, mkAttrElemC, optionalContent)

newtype ActionOverridePackage = ActionOverridePackage { actionOverrides :: ActionOverride } deriving (Generic, Show)
data ActionName = Accept |  Clone | Delete | Edit | List | New | Tab | View deriving (Generic, Read, Show) 
data ActionType = Default | Flexipage | Lightningcomponent | Scontrol | Standard | Visualforce deriving (Generic, Read, Show) 
data FormFactor = Large | Medium | Small deriving (Generic, Show, Read)

data ActionOverride = ActionOverride
    { actionName           :: ActionName
    , actionType           :: ActionType
    , comment              :: Maybe Text
    , content              :: Maybe Text
    , formFactor           :: Maybe FormFactor
    , skipRecordTypeSelect :: Maybe Bool
    } deriving (Generic, Show)


instance ToJSON ActionOverridePackage where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }
instance ToJSON ActionName where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }

instance ToJSON ActionType where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }

instance ToJSON FormFactor where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }

instance ToJSON ActionOverride where
    toJSON = genericToJSON defaultOptions
        { omitNothingFields = True }

instance FromJSON ActionOverridePackage
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

instance HTypeable Text where
    toHType x = Defined (T.unpack x) [] []

instance HTypeable ActionName where
    toHType a = Defined (strToLower a) [] [Constr "actionName" []  []]

instance HTypeable ActionType where
    toHType a = Defined (strToLower a) [] [Constr "type" []  []] 

instance HTypeable FormFactor where
    toHType a = Defined (show a) [] [Constr "formFactor" []  []] 

instance HTypeable ActionOverride where
    toHType (ActionOverride an at cmt ctn ff srts) = Defined "actionOverrides" [] [Constr "actionOverrides" [] [toHType an, toHType at, toHType cmt, toHType ctn, toHType ff, toHType srts]]

instance XmlContent ActionName where 
    parseContents = fmap (read . capitalize) $ inElement "actionName" text

    toContents v =
        [mkAttrElemC (showConstr 0 $ toHType v) [] (toText . value $ toHType v)]
        where value (Defined v' _ _) = v'

instance XmlContent ActionType where 
    parseContents = fmap (read . capitalize) $ inElement "type" text

    toContents v =
        [mkAttrElemC (showConstr 0 $ toHType v) [] (toText . value $ toHType v)]
        where value (Defined v' _ _) = v'

instance XmlContent FormFactor where 
    parseContents = fmap read $ inElement "formFactor" text
    toContents v =
        [mkAttrElemC (showConstr 0 $ toHType v) [] (toText . value $ toHType v)]
        where value (Defined v' _ _) = v'

instance XmlContent ActionOverride where
    parseContents = do 
            e <- element ["actionOverrides"] 
            interior e (ActionOverride <$> parseContents <*> parseContents <*> parseMaybeCm <*> parseMaybeC <*> parseContents <*> parseMaybeSkip)
        where 
            parseMaybeCm = optional $ fmap (T.pack) $ inElement "comment" text
            parseMaybeC = optional $ fmap (T.pack) $ inElement "content" text
            parseMaybeSkip = optional $ fmap (const True) $ inElementWith (\x y -> True) "skipRecordTypeSelect" text
    toContents v@(ActionOverride an at cm ct ff sr) = toContents an ++ toContents at ++ optionalContent "comment" cm ++ optionalContent "content" ct ++ toContents ff ++ optionalContent "skipRecordTypeSelect" sr
        where value (Defined _ _ [Constr _  _ v']) = v'
              d v i = Defined v [] [Constr i []  []]

