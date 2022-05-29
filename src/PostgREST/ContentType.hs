{-# LANGUAGE DuplicateRecordFields #-}

module PostgREST.ContentType
  ( ContentType(..)
  , TemplateName(..)
  , toHeader
  , toMime
  , decodeContentType
  ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS (c2w)

import qualified Data.Text.Encoding as Text

import Network.HTTP.Types.Header (Header, hContentType)

import Protolude
import Data.List (lookup)

-- | Enumeration of currently supported response content types
data ContentType
  = CTApplicationJSON
  | CTSingularHTML (Maybe TemplateName)
  | CTSingularJSON
  | CTTextCSV
  | CTTextHTML (Maybe TemplateName)
  | CTTextPlain
  | CTTextXML
  | CTOpenAPI
  | CTUrlEncoded
  | CTOctetStream
  | CTAny
  | CTOther ByteString
  deriving (Eq)

-- | Convert from ContentType to a full HTTP Header
toHeader :: ContentType -> Header
toHeader ct = (hContentType, toMime ct <> charset)
  where
    charset = case ct of
      CTOctetStream -> mempty
      CTOther _     -> mempty
      _             -> "; charset=utf-8"

-- | Convert from ContentType to a ByteString representing the mime type
toMime :: ContentType -> ByteString
toMime CTApplicationJSON  = "application/json"
toMime CTTextCSV          = "text/csv"
toMime (CTTextHTML _)     = "text/html"
toMime CTTextPlain        = "text/plain"
toMime CTTextXML          = "text/xml"
toMime CTOpenAPI          = "application/openapi+json"
toMime (CTSingularHTML _) = "application/vnd.pgrst.object+html"
toMime CTSingularJSON     = "application/vnd.pgrst.object+json"
toMime CTUrlEncoded       = "application/x-www-form-urlencoded"
toMime CTOctetStream      = "application/octet-stream"
toMime CTAny              = "*/*"
toMime (CTOther ct)       = ct

-- | Convert from ByteString to ContentType. Warning: discards MIME parameters
decodeContentType :: BS.ByteString -> ContentType
decodeContentType ct =
  let
    contentType = BS.takeWhile (/= BS.c2w ';') ct
    mimeParameters =
      map (\[p, v] -> (p, v))           -- Turn lists into pairs
      $ filter (\l -> length l == 2)    -- Remove lists that aren't exactly two values long
      $ map (BS.split $ BS.c2w '=')     -- Split MIME parameters into parameter & value lists
      $ BS.split (BS.c2w ';')           -- Split up MIME parameters
      $ BS.dropWhile (/= BS.c2w ';') ct -- Remove content type
  in
    case contentType of
      "application/json"                  -> CTApplicationJSON
      "text/csv"                          -> CTTextCSV
      "text/html"                         ->
        CTTextHTML (TemplateName <$> (lookup "template" mimeParameters >>= (eitherToMaybe . Text.decodeUtf8')))
      "text/plain"                        -> CTTextPlain
      "text/xml"                          -> CTTextXML
      "application/openapi+json"          -> CTOpenAPI
      "application/vnd.pgrst.object+html" ->
        CTSingularHTML (TemplateName <$> (lookup "template" mimeParameters >>= (eitherToMaybe . Text.decodeUtf8')))
      "application/vnd.pgrst.object+json" -> CTSingularJSON
      "application/vnd.pgrst.object"      -> CTSingularJSON
      "application/x-www-form-urlencoded" -> CTUrlEncoded
      "application/octet-stream"          -> CTOctetStream
      "*/*"                               -> CTAny
      ct'                                 -> CTOther ct'

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Right r) = Just r
eitherToMaybe _ = Nothing

newtype TemplateName = TemplateName { unTemplateName :: Text }
  deriving (Eq, Ord, Show)
