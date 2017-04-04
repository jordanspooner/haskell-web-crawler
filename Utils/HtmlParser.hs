{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils.HtmlParser where

import Data.Char (isAlpha, isSpace, toLower)
import Data.Monoid ((<>))
import Utils.UrlParser (Url, showUrl, formatMaybeAssetUrls, formatMaybeLinkedUrls)
import Data.Aeson
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------------------------------------
-- WEBPAGE DATA TYPE

-- | A container for webpages, made up of the page URL, the URLs  of its assets,
--   and tthe URLs of any webpages it links to.
data Webpage = Webpage { url    :: Url   -- ^ The URL for the webpage
                       , assets :: [Url] -- ^ The list of URLs of linked assets
                       , links  :: [Url] -- ^ The list of URLs of linked pages
                       } deriving (Show, Eq, Ord)

instance ToJSON L.ByteString where
  toJSON
    = toJSON . decodeUtf8 . B.concat . L.toChunks

instance ToJSON Webpage where
 toJSON (Webpage pageUrl pageAssets _)
   = object ["url" .= showUrl pageUrl, "assets" .= map showUrl pageAssets]
 toEncoding (Webpage pageUrl pageAssets _)
   = pairs ("url" .= showUrl pageUrl <> "assets" .= map showUrl pageAssets)

--------------------------------------------------------------------------------
-- PARSING FUNCTIONS

-- | Returns a Webpage object with asset and page links for a given URL and HTML
--   source.
crawlWebpage :: Url             -- ^ The URL of the page to be crawled
                -> L.ByteString -- ^ The HTML source for the page
                -> Webpage      -- ^ Returns a Webpage object with links
crawlWebpage currentUrl bs
  = Webpage currentUrl (formatMaybeAssetUrls pageAssets currentUrl)
    (formatMaybeLinkedUrls pageLinks currentUrl)
  where
    (pageAssets, pageLinks) = parseHtml bs [] []

-- | Given HTML source, accumulates seperately links for assets and other pages.
--   Accumulators should usually be initiated as empty lists.
--   Any links are wrapped in "Just", tags without links produce "Nothing".
parseHtml :: L.ByteString               -- ^ The HTML source to be parsed
             -> [Maybe L.ByteString]    -- ^ The accumulator for asset links
             -> [Maybe L.ByteString]    -- ^ The accumulator for page links
             -> ([Maybe L.ByteString]
                , [Maybe L.ByteString]) -- ^ Returns tuple containing list of
                                        --   all Maybe asset links and list of
                                        --   all Maybe page links
parseHtml bs as ls
  | L.null bs
    = (as, ls)
  | L.head bs == '<' && L.map toLower tag `elem` assetTags
    = parseHtml rest' (maybeUrl : as) ls
  | L.head bs == '<' && L.map toLower tag `elem` linkTags
    = parseHtml rest' as (maybeUrl : ls)
  | otherwise
    = parseHtml (L.tail bs) as ls
  where
    (tag, rest)       = L.span isAlpha $ L.tail bs
    (maybeUrl, rest') = parseAttributes rest

-- | Given a bytestring beginning with a list of attributes, returns Just the
--   first value which refers to a link, or if no such value exists, Nothing.
--   Also returns the remainder of the bytestring to be parsed.
parseAttributes :: L.ByteString           -- ^ The bytestring to be parsed
                   -> (Maybe L.ByteString
                      , L.ByteString)     -- ^ Returns tuple containing Maybe
                                          --   link value and remainder of
                                          --   bytestring to be parsed
parseAttributes bs
  | L.null bs        = (Nothing, bs) -- Missing '>' character
  | L.head bs == '>' = (Nothing, bs) -- No assets or links found
  | otherwise        = if L.map toLower name `elem` attributeNames
                       then (Just value, rest'')
                       else parseAttributes rest''
  where
    (name, rest)   = parseName $ dropSpace bs
    (value, rest') = parseValue $ dropSpace . dropEquals . dropSpace $ rest
    rest''         = dropQuote . dropSpace $ rest'

-- | Given a bytestring beginning with an attribute and *with no preceeding
--   whitespace*, returns the name of the attribute and the remainder of the
--   bytestring to be parsed.
parseName :: L.ByteString       -- ^ The bytestring to be parsed
             -> (L.ByteString
                , L.ByteString) -- ^ Returns the name of the first attribute
                                --   and remainder of bytestring to be parsed
parseName
  = L.break (\c -> c `elem` ['=', '>'] || isSpace c)

-- | Given a bytestring beginning with an attribute value and *with no
--   preceeding whitespace*, returns the attribute value and the remainder of
--   the bytestring to be parsed.
parseValue :: L.ByteString     -- ^ The bytestring to be parsed
              -> (L.ByteString
              , L.ByteString)  -- ^ Returns the first attribute value and the
                               --   remainder of the bytestring to be parsed
parseValue bs
  | L.head bs == '\'' = L.break (== '\'') $ L.tail bs
  | L.head bs == '\"' = L.break (== '\"') $ L.tail bs
  | otherwise         = L.break (\c -> c == '>' || isSpace c) bs

--------------------------------------------------------------------------------
-- PARSER HELPER FUNCTIONS

-- | Removes any whitespace from the beginning of a bytestring
dropSpace :: L.ByteString -> L.ByteString
dropSpace
  = L.dropWhile isSpace

-- | Removes any equal signs from the beginning of a bytestring
dropEquals :: L.ByteString -> L.ByteString
dropEquals
  = L.dropWhile (== '=')

-- | Removes any quotation marks from the beginning of a bytestring
dropQuote :: L.ByteString -> L.ByteString
dropQuote
  = L.dropWhile (`elem` ['\'', '\"'])

-- | HTML tags that contain links to assets
assetTags :: [L.ByteString]
assetTags
  = ["link", "script", "img", "video", "source", "audio", "object", "embed"]

-- | HTML tags that contain links to pages
linkTags :: [L.ByteString]
linkTags
  = ["a", "iframe"]

-- | HTML attribute names that refer to assets and links
attributeNames :: [L.ByteString]
attributeNames
  = ["href", "src", "data"]
