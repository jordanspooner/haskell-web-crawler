{-# LANGUAGE OverloadedStrings #-}

module Utils.UrlParser where

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

isValid :: L.ByteString -> Bool
isValid url
  = L.take 7 url /= "mailto:"
    && L.take 11 url /= "javascript:"

--------------------------------------------------------------------------------
-- URL FORMATTING
-- Functions to convert URLs into a standard universal format

formatMaybeUrls :: L.ByteString -> [Maybe L.ByteString] -> [L.ByteString]
formatMaybeUrls url urls
  = map (formatUrl url) $ nub $ filter isValid $ catMaybes urls

formatUrl :: L.ByteString -> L.ByteString -> L.ByteString
formatUrl currentUrl url
  | L.null url
    = currentUrl
  -- Absolute links
  | L.take 7 url == "http://"
    = url
  | L.take 8 url == "https://"
    = L.append "http://" $ L.drop 8 url
  | L.take 2 url == "//"
    = L.append "http://" $ L.drop 2 url
  | L.take 4 url == "www."
    = L.append "http://" url
  -- Relative links
  | L.take 2 url == "./"
    = formatUrl currentUrl $ L.drop 2 url
  | L.take 3 url == "../"
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
  = L.append "http://" $ L.takeWhile (/= '/') $ L.drop 7 url

getParent :: L.ByteString -> L.ByteString
-- Pre: url is formatted correctly (i.e. by formatUrl)
-- Post: *does* include a final '/' character
getParent url
  -- Check if we have reached current URL, if so finish
  | L.null next    = ""
  -- Check for adjacent '/' characters, to prevent infinite loop
  | L.null current = L.append "/" (getParent $ L.tail next)
  -- Otherwise add this directory to return string
  | otherwise      = L.append current (getParent next)
  where
    (current, next) = L.span (/= '/') url
