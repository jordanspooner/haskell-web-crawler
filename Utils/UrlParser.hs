{-# LANGUAGE OverloadedStrings #-}

module Utils.UrlParser where

import Data.Maybe (fromJust, mapMaybe, catMaybes)
import Data.List (nub)
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, unpack)

--------------------------------------------------------------------------------
-- URL FORMATTING FUNCTIONS
-- Format lists of urls so that they are absolute and do not include duplicates

formatMaybeAssetUrls :: [Maybe L.ByteString] -> String -> [String]
-- Pre: thisUrl is a well-formed URI
formatMaybeAssetUrls maybeUrls thisUrl
  = nub . mapMaybe (fmap showUrl . (`parseUrl` url)) $ catMaybes maybeUrls
  where
    url = fromJust $ parseURIReference' thisUrl

formatMaybeLinkedUrls :: [Maybe L.ByteString] -> String -> [String]
-- Pre: thisUrl is a well-formed and absolute URI
formatMaybeLinkedUrls maybeUrls thisUrl
  = nub . map showUrl . filter (isValidLinkedUrl url)
    . mapMaybe (`parseUrl` url) $ catMaybes maybeUrls
  where
    url = fromJust $ parseURIReference' thisUrl

--------------------------------------------------------------------------------

parseURIReference' :: String -> Maybe URI
parseURIReference' str
  | take 4 str == "www." = (fmap fixPath . parseURIReference . ("http://" ++))
                           str
  | otherwise            = (fmap fixPath . parseURIReference) str

fixPath :: URI -> URI
fixPath URI { uriScheme = thisScheme
            , uriAuthority = thisAuthority
            , uriPath = thisPath
            }
  = if thisPath == ""
    then URI thisScheme thisAuthority "/" "" ""
    else URI thisScheme thisAuthority thisPath "" ""

--------------------------------------------------------------------------------
-- URL HELPER FUNCTIONS
-- Functions for parsing URLs, checking they are valid, and showing them

parseUrl :: L.ByteString -> URI -> Maybe URI
-- Post: returned URI is absolute (includes authority)
parseUrl linkedUrl thisUrl
  = fmap (\url -> if uriIsRelative url
                  then url `relativeTo` thisUrl
                  else url)
         maybeLinkedUrl
    where
      maybeLinkedUrl = parseURIReference' $ L.unpack linkedUrl

isValidLinkedUrl :: URI -> URI -> Bool
-- Pre: URLs are absolute
isValidLinkedUrl URI {uriScheme = linkedScheme, uriAuthority = linkedAuth}
                 URI {uriAuthority = thisAuth}
  = (linkedScheme == "http:" || linkedScheme == "https:")
    && linkedAuth == thisAuth

showUrl :: URI -> String
-- Post: Queries and tags not shown in output string
showUrl URI { uriScheme = thisScheme
            , uriAuthority = thisAuthority
            , uriPath = thisPath
            }
  = (thisScheme ++) . ("//" ++) . (uriRegName auth ++) . (uriPort auth ++)
    $ thisPath
  where
    auth = fromJust thisAuthority

equalsUrl :: String -> String -> Bool
-- Pre: URLs are well-formed and absolute
equalsUrl url1 url2
  = auth1 == auth2 && path1 == path2
  where
    URI {uriAuthority = auth1, uriPath = path1}
      = fromJust $ parseURIReference' url1
    URI {uriAuthority = auth2, uriPath = path2}
      = fromJust $ parseURIReference' url2
