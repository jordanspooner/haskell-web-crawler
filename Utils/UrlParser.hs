{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.UrlParser where

import Data.Maybe (isJust, fromJust, fromMaybe, catMaybes)
import Data.List (nub)
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as L

data Url = Url { scheme    :: L.ByteString
               , authority :: L.ByteString
               , path      :: L.ByteString
               }

instance Show Url where
  show (Url urlScheme urlAuth urlPath)
    = concatMap L.unpack [urlScheme, "//", urlAuth, urlPath]

instance Eq Url where
  (==) (Url _ auth1 path1) (Url _ auth2 path2)
    = auth1 == auth2 && path1 == path2

--------------------------------------------------------------------------------

formatMaybeAssetUrls :: [Maybe L.ByteString] -> Url -> [Url]
-- Pre: thisUrl is a well-formed URI
formatMaybeAssetUrls maybeUrls thisUrl
  = nub . map (`parseUrlRelativeTo` thisUrl) $ catMaybes maybeUrls

formatMaybeLinkedUrls :: [Maybe L.ByteString] -> Url -> [Url]
-- Pre: thisUrl is a well-formed and absolute URI
formatMaybeLinkedUrls maybeUrls thisUrl
  = filter (`isValidLinkedUrl` thisUrl) . (`formatMaybeAssetUrls` thisUrl)
    $ maybeUrls

--------------------------------------------------------------------------------

parseUrl :: L.ByteString -> Url
parseUrl bs
  | isAbsoluteUrl = Url (fromMaybe "http:" s) (L.map toLower $ fromJust a)
                        (normalise p)
  | otherwise     = Url (fromMaybe "http:" s) (L.map toLower p) "/"
  where
    isAbsoluteUrl = isJust a
    (s, rest)     = parseScheme bs
    (a, rest')    = parseAuthority rest
    p             = parsePath rest'

parseUrlRelativeTo :: L.ByteString -> Url -> Url
parseUrlRelativeTo linkedUrlBs (Url thisScheme thisAuth thisPath)
  = Url (fromMaybe thisScheme linkedScheme)
        (L.map toLower $ fromMaybe thisAuth linkedAuth)
        (normalise $ if isAbsolutePath then linkedPath
                     else L.append thisPath linkedPath)
  where
    (linkedScheme, rest) = parseScheme linkedUrlBs
    (linkedAuth, rest')  = parseAuthority rest
    linkedPath           = parsePath rest'
    isAbsolutePath       = L.head linkedPath == '/'

parseScheme :: L.ByteString -> (Maybe L.ByteString, L.ByteString)
parseScheme bs
  | maybeScheme' == "http:"  = (Just maybeScheme', rest')
  | maybeScheme' == "https:" = (Just maybeScheme', rest')
  | L.null maybeScheme       = (Nothing, bs)
  | L.null rest              = (Nothing, maybeScheme)
  | otherwise                = (Nothing, "")
  where
    (maybeScheme, rest) = L.break (== ':') bs
    maybeScheme'        = L.append (L.map toLower maybeScheme) ":"
    rest'               = if L.null rest then "" else L.tail rest

parseAuthority :: L.ByteString -> (Maybe L.ByteString, L.ByteString)
parseAuthority bs
  | doubleSlash == "//"             = parseAuthority auth
  | L.null rest && wwwDot == "www." = (Just bs, "")
  | L.null maybeAuth                = (Nothing, rest)
  | L.null rest                     = (Nothing, maybeAuth)
  | otherwise                       = (Just maybeAuth, rest)
  where
    (doubleSlash, auth) = L.splitAt 2 bs
    (wwwDot, _)         = L.splitAt 4 bs
    (maybeAuth, rest)   = L.break (`elem` endAuthChars) bs

parsePath :: L.ByteString -> L.ByteString
parsePath bs
  | L.null bs        = "/"
  | firstChar == '?' = "/"
  | firstChar == '#' = "/"
  | firstChar == '/' = maybePath
  | otherwise        = bs
  where
    firstChar = L.head bs
    maybePath = L.takeWhile (`notElem` endPathChars) bs

normalise :: L.ByteString -> L.ByteString
normalise bs
  = foldl (flip L.append) "" $ normalise' bs []

normalise' :: L.ByteString -> [L.ByteString] -> [L.ByteString]
normalise' "" accum
  = accum
normalise' input accum
  | take3 == "/.."          = normalise' drop3 $ if null accum
                                                   then []
                                                   else tail accum
  | take2 == "/."           = normalise' drop2 accum
  | take11 == "/index.html" = normalise' drop11 accum
  | otherwise               = normalise' rest $ L.cons '/' this : accum
  where
    (take2, drop2)   = L.splitAt 2 input
    (take3, drop3)   = L.splitAt 3 input
    (take11, drop11) = L.splitAt 11 input
    (this, rest)     = L.break (== '/') $ L.tail input

endAuthChars :: String
endAuthChars
  = "/?#"

endPathChars :: String
endPathChars
  = "?#"

--------------------------------------------------------------------------------

isValidLinkedUrl :: Url -> Url -> Bool
isValidLinkedUrl linkedUrl thisUrl
  = authority linkedUrl == authority thisUrl

showUrl :: Url -> L.ByteString
showUrl (Url urlScheme urlAuth urlPath)
  = L.concat [urlScheme, "//", urlAuth, urlPath]
