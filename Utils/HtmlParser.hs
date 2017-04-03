{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils.HtmlParser where

import Data.Char (isAlpha, isSpace)
import Data.Monoid ((<>))
import Utils.UrlParser (formatMaybeAssetUrls, formatMaybeLinkedUrls)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L

data Webpage   = Webpage { url    :: String
                         , assets :: [String]
                         , links  :: [String]
                         } deriving (Show)

instance ToJSON Webpage where
 toJSON (Webpage pageUrl pageAssets _)
   = object ["url" .= pageUrl, "assets" .= pageAssets]
 toEncoding (Webpage pageUrl pageAssets _)
   = pairs ("url" .= pageUrl <> "assets" .= pageAssets)

--------------------------------------------------------------------------------
-- PARSING FUNCTIONS
-- Parses HTML source to find links to assets and other webpages

crawlWebpage :: String -> L.ByteString -> Webpage
crawlWebpage currentUrl bs
  = Webpage currentUrl (formatMaybeAssetUrls pageAssets currentUrl)
    (formatMaybeLinkedUrls pageLinks currentUrl)
  where
    (pageAssets, pageLinks) = parseHtml bs [] []

parseHtml :: L.ByteString -> [Maybe L.ByteString] -> [Maybe L.ByteString]
             -> ([Maybe L.ByteString], [Maybe L.ByteString])
parseHtml bs as ls
  | L.null bs
    = (as, ls)
  | L.head bs == '<' && tag `elem` assetTags
    = parseHtml rest' (maybeUrl : as) ls
  | L.head bs == '<' && tag `elem` linkTags
    = parseHtml rest' as (maybeUrl : ls)
  | otherwise
    = parseHtml (L.tail bs) as ls
  where
    (tag, rest)       = L.span isAlpha $ L.tail bs
    (maybeUrl, rest') = parseAttributes rest

parseAttributes :: L.ByteString -> (Maybe L.ByteString, L.ByteString)
parseAttributes bs
  | L.null bs        = (Nothing, bs)        -- Missing '>' character
  | L.head bs == '>' = (Nothing, L.tail bs) -- No assets or links found
  | otherwise        = if name `elem` attributeNames
                       then (Just value, rest'')
                       else parseAttributes rest''
  where
    (name, rest)   = parseName $ dropSpace bs
    (value, rest') = parseValue $ dropSpace . dropEquals . dropSpace $ rest
    rest''         = dropQuote . dropSpace $ rest'

parseName :: L.ByteString -> (L.ByteString, L.ByteString)
-- Pre: no leading whitespace
parseName
  = L.break (\c -> c `elem` ['=', '>'] || isSpace c)

parseValue :: L.ByteString -> (L.ByteString, L.ByteString)
-- Pre: no leading whitespace
parseValue bs
  | L.head bs == '\'' = L.break (== '\'') $ L.tail bs
  | L.head bs == '\"' = L.break (== '\"') $ L.tail bs
  | otherwise         = L.break (\c -> c == '>' || isSpace c) bs

--------------------------------------------------------------------------------
-- TAGS and ATTRIBUTES
-- List of HTML tags that contain links, and the relevant attribute names

assetTags :: [L.ByteString]
assetTags
  = ["link", "script", "img", "video", "source", "audio", "object", "embed"]

linkTags :: [L.ByteString]
linkTags
  = ["a", "iframe"]

attributeNames :: [L.ByteString]
attributeNames
  = ["href", "src", "data"]

--------------------------------------------------------------------------------
-- PARSER HELPER FUNCTIONS
-- Functions to easily remove spaces, equals signs and quotation marks

dropSpace :: L.ByteString -> L.ByteString
dropSpace
  = L.dropWhile isSpace

dropEquals :: L.ByteString -> L.ByteString
dropEquals
  = L.dropWhile (== '=')

dropQuote :: L.ByteString -> L.ByteString
dropQuote
  = L.dropWhile (`elem` ['\'', '\"'])
