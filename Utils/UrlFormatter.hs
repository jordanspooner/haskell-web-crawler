module Utils.UrlFormatter where

import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------------------------------------
-- URL CHECKING
-- Functions to check the validity of links

formatMaybeLinkUrls :: L.ByteString -> [Maybe L.ByteString] -> [L.ByteString]
formatMaybeLinkUrls url urls
  = filter (checkLinkUrl url) $ formatMaybeUrls url urls

checkLinkUrl :: L.ByteString -> L.ByteString -> Bool
checkLinkUrl currentUrl url
  = getSubdomain currentUrl == getSubdomain url

isMailto :: L.ByteString -> Bool
isMailto url
  = L.take 7 url == L.pack "mailto:"

--------------------------------------------------------------------------------
-- URL FORMATTING
-- Functions to convert URLs into a standard universal format

formatMaybeUrls :: L.ByteString -> [Maybe L.ByteString] -> [L.ByteString]
formatMaybeUrls url urls
  = map (formatUrl url) $ nub $ filter (not . isMailto) $ catMaybes urls

formatUrl :: L.ByteString -> L.ByteString -> L.ByteString
formatUrl currentUrl url
  | L.null url
    = currentUrl
  -- Absolute links
  | L.take 7 url == L.pack "http://"
    = url
  | L.take 8 url == L.pack "https://"
    = L.append (L.pack "http://") $ L.drop 8 url
  | L.take 2 url == L.pack "//"
    = L.append (L.pack "http://") $ L.drop 2 url
  | L.take 4 url == L.pack "www."
    = L.append (L.pack "http://") url
  -- Relative links
  | L.take 2 url == L.pack "./"
    = formatUrl currentUrl $ L.drop 2 url
  | L.take 3 url == L.pack "../"
    = formatUrl (getParent currentUrl) $ L.drop 3 url
  | L.head url == '/'
    = L.append (getSubdomain currentUrl) url
  | otherwise
    = L.append (getParent currentUrl) url

--------------------------------------------------------------------------------
-- CURRENT URL HELPER FUNCTIONS
-- Functions to find current subdomain and parent directory

getSubdomain :: L.ByteString -> L.ByteString
-- Pre: url is formatted correctly (i.e. by formatUrl)
-- Post: does *not* include a final '/' character
getSubdomain url
  = L.append (L.pack "http://") $ L.takeWhile (/= '/') $ L.drop 7 url

getParent :: L.ByteString -> L.ByteString
-- Pre: url is formatted correctly (i.e. by formatUrl)
-- Post: *does* include a final '/' character
getParent url
  -- Check if we have reached current URL, if so finish
  | L.null next    = L.pack ""
  -- Check for adjacent '/' characters, to prevent infinite loop
  | L.null current = L.append (L.pack "/") (getParent $ L.tail next)
  -- Otherwise add this directory to return string
  | otherwise      = L.append current (getParent next)
  where
    (current, next) = L.span (/= '/') url
