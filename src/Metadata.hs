{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Metadata
    ( module Metadata.ActionOverride
    , module Metadata.CustomObject
    , Metadata(..)
    , decodeMeta
    , encodeMeta
    )
    where

import GHC.Generics (Generic)
import Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Data.Text (Text)
import qualified Data.Text as T 
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Lazy as HM    
import Data.Monoid ((<>))
import Text.XML.HaXml.XmlContent hiding (Parser, Success)
import Metadata.Commons (mkAttrElemC)
import Metadata.ActionOverride
import Metadata.CustomObject 

data Metadata 
    = CustomObjectMeta CustomObject
    | ActionOverrideMeta ActionOverride
    | BadMetadata Text
    deriving (Generic, Show)

instance HTypeable Metadata where
    toHType _ = toHType ("Metadata" :: String)

instance XmlContent Metadata where
    parseContents = error "parseContents not yet implemented."
    toContents = \case
        (ActionOverrideMeta x) ->
            [mkAttrElemC (showConstr 0 $ toHType x) [mkAttr "xmlns" "http://soap.sforce.com/2006/04/metadata"] (toContents x)]
        _ -> error "Bad type"
        

instance FromJSON Metadata where
      parseJSON = withObject "Metadata" metadataType
        where metadataType o = parseMetadata . metadataValidInput . headMaybe . HM.toList $ o
              metadataValidInput Nothing = (Nothing, Nothing)
              metadataValidInput (Just (x,y)) = (Just x, Just y)
              parseMetadata x =  uncurry parseMetadata' x

parseMetadata' :: Maybe Text -> Maybe Value -> Parser Metadata 
parseMetadata' _ Nothing = parseMetadataFail 
parseMetadata' Nothing _ = parseMetadataFail
parseMetadata' (Just metatype) (Just o) = case metatype of
    "customObject"    -> parseResult CustomObjectMeta $ fromJSON o
    "actionOverrides" -> parseResult ActionOverrideMeta $ fromJSON o
    _                -> return $ BadMetadata ("'" <> metatype <> "' not a valid metadata.")

parseMetadataFail = return $ BadMetadata "metadata object attribute name not found"

parseResult f (Success meta) = return $ f meta
parseResult _ (Error str)    = return $ BadMetadata $ T.pack str

decodeMeta :: BL.ByteString -> Maybe Metadata
decodeMeta = decoder . decode
    where decoder (Just (BadMetadata _)) = Nothing
          decoder x = x

encodeMeta :: Metadata -> BL.ByteString 
encodeMeta = fpsShowXml False

headMaybe :: [(Text, Value)] -> Maybe (Text, Value)
headMaybe [] = Nothing
headMaybe ((x,y):_) = Just (x, y)
