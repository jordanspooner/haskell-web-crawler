module Utils.HtmlParser where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
import Utils.UrlFormatter

type Url     = L.ByteString
type Asset   = L.ByteString
data Webpage = Webpage { url    :: Url
                       , assets :: [Asset]
                       , links  :: [Url]
                       } deriving (Show)

--------------------------------------------------------------------------------
-- PARSING FUNCTIONS
-- Parses HTML source to find links to assets and other webpages

crawlWebpage :: Url -> L.ByteString -> Webpage
crawlWebpage url bs
  = Webpage url (formatMaybeUrls url assets) (formatMaybeLinkUrls url links)
  where
    (assets, links) = parseHtml bs [] []

parseHtml :: L.ByteString -> [Maybe Asset] -> [Maybe Url]
             -> ([Maybe Asset], [Maybe Url])
parseHtml bs assets links
  | L.null bs
    = (assets, links)
  | L.head bs == '<' && tag `elem` assetTags
    = parseHtml rest' (maybeUrl : assets) links
  | L.head bs == '<' && tag `elem` linkTags
    = parseHtml rest' assets (maybeUrl : links)
  | otherwise
    = parseHtml (L.tail bs) assets links
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
  = map L.pack
    ["link", "script", "img", "video", "source", "audio", "object", "embed"]

linkTags :: [L.ByteString]
linkTags
  = map L.pack
    ["a", "iframe"]

attributeNames :: [L.ByteString]
attributeNames
  = map L.pack
    ["href", "src", "data"]

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