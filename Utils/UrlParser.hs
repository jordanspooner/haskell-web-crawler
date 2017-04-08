{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.UrlParser where

import           Data.Char                  (ord, toLower)
import           Data.List                  (nub)
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe,
                                             isJust)
import           Numeric                    (showHex)

import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------------------------------------
-- URL DATA TYPE

-- | A container for URLs, made up of scheme, authority and path.
data Url = Url { scheme    :: L.ByteString -- ^ E.g. "http:"
               , authority :: L.ByteString -- ^ E.g. "www.example.com"
               , path      :: L.ByteString -- ^ E.g. "\/dir\/page.html"
               } deriving (Ord)

-- | E.g. Show (Url "http:" "example.com" "\/path") returns the String
--   "http:\/\/example.com\/path".
instance Show Url where
  show (Url urlScheme urlAuth urlPath)
    = concatMap L.unpack [urlScheme, "//", urlAuth, escape urlPath]

-- | Two Urls are considered equal if their authorities and paths are equal.
instance Eq Url where
  (==) (Url _ auth1 path1) (Url _ auth2 path2)
    = auth1 == auth2 && path1 == path2

--------------------------------------------------------------------------------
-- FUNCTIONS to PARSE URL LISTS

-- | Parses a list of Maybe linked URLs relative to a given current URL. Returns
--   a list of URL objects for valid asset links.
formatMaybeAssetUrls :: [Maybe L.ByteString] -- ^ List of Maybe asset URLs to
                                             --   parse
                        -> Url               -- ^ The current URL
                        -> [Url]             -- ^ Returns list of parsed asset
                                             --   URLs
formatMaybeAssetUrls maybeUrls thisUrl
  = nub . map (`parseUrlRelativeTo` thisUrl) $ catMaybes maybeUrls

-- | Parses a list of Maybe linked URLs relative to a given current URL. Returns
--   a list of URL objects for valid links within the same subdomain.
formatMaybeLinkedUrls :: [Maybe L.ByteString] -- ^ List of Maybe linked URLs to
                                              --   parse
                        -> Url                -- ^ The current URL
                        -> [Url]              -- ^ Returns list of parsed linked
                                              --   URLs
formatMaybeLinkedUrls maybeUrls thisUrl
  = filter (`isValidLinkedUrl` thisUrl) . (`formatMaybeAssetUrls` thisUrl)
    $ maybeUrls

--------------------------------------------------------------------------------
-- URL PARSING FUNCTIONS

-- | Parses a bytestring representation of a URL into a Url object.
--   Scheme is "http:" unless specified as "https:". Returns "http:\/\/\/" for
--   any other scheme. Will always produce an absolute URL, including authority.
--   Returned scheme and authority are lowercase and path is normalised.
parseUrl :: L.ByteString -- ^ The bytestring URL to be parsed
            -> Url       -- ^ Returns the parsed Url
parseUrl bs
  | isAbsoluteUrl = Url (fromMaybe "http:" s) (L.map toLower $ fromJust a)
                        (normalise p)
  | otherwise     = Url (fromMaybe "http:" s) (L.map toLower p) "/"
  where
    isAbsoluteUrl = isJust a
    (s, rest)     = parseScheme bs
    (a, p)        = parseAuthority rest

-- | Parses a bytestring for a linked URL into a Url object, by considering it
--   relative to a given current URL.
--   If no scheme or authority is specified for the linked URL, uses the current
--   URL to guess.
--   The linked URL will be considered relative (and the path added to that of
--   the current url) iff it:
--
--   * does not begin with "www."
--   * AND does not have a \'\/\' except for as the first character
--      or does not have a \'.\' before a \'\/\' except for as the first
--      character
--
--   Returned scheme and authority are lowercase and path is normalised.
parseUrlRelativeTo :: L.ByteString -- ^ The linked URL, to be parsed
                      -> Url       -- ^ The current URL (parent)
                      -> Url       -- ^ Returns the linked URL parsed relative
                                   --   to the current URL
parseUrlRelativeTo linkedUrlBs (Url thisScheme thisAuth thisPath)
  = Url (fromMaybe thisScheme linkedScheme)
        (L.map toLower $ fromMaybe thisAuth linkedAuth)
        (normalise $ if isAbsolutePath then linkedPath
                     else L.append thisPath $ L.cons '/' linkedPath)
  where
    (linkedScheme, rest)     = parseScheme linkedUrlBs
    (linkedAuth, linkedPath) = parseAuthority rest
    isAbsolutePath           = L.take 1 linkedPath == "/"

-- | Parses a bytestring to retrieve its scheme if it is "http:" or "https:"
--   and the remainder to be parsed.
--   If no scheme is found, returns Nothing and the entire URL. If an incorrect
--   scheme is found (e.g. "mailto:") returns Nothing and an empty string.
parseScheme :: L.ByteString           -- ^ The URL to be parsed
               -> (Maybe L.ByteString
                  , L.ByteString)     -- ^ Returns tuple of Maybe scheme and
                                      --   remainder of URL
parseScheme bs
  | maybeScheme' == "http:"  = (Just maybeScheme', rest')
  | maybeScheme' == "https:" = (Just maybeScheme', rest')
  | L.null maybeScheme       = (Nothing, bs)
  | L.null rest              = (Nothing, maybeScheme)
  | otherwise                = (Nothing, "")
  where
    (maybeScheme, rest) = L.break (== ':') bs
    maybeScheme'        = L.append (L.map toLower maybeScheme) ":"
    rest'               = L.drop 1 rest

-- | Parses a bytestring with scheme removed, to retrieve its authority and the
--   and.
--   If no authority is found, returns Nothing and the entire URL.
parseAuthority :: L.ByteString           -- ^ The URL (without scheme) to be
                                         --   parsed
                  -> (Maybe L.ByteString
                     , L.ByteString)     -- ^ Returns tuple of Maybe authority
                                         --   and remainder of the URL
parseAuthority bs
  | doubleSlash == "//"             = parseAuthority' auth
  | wwwDot == "www." && L.null rest = (Just bs, "")
  | L.take 1 bs == "." || '.' `notElem` L.unpack maybeAuth || L.null maybeAuth
    || L.null rest                  = (Nothing, bs)
  | otherwise                       = (Just maybeAuth, rest)
  where
    (doubleSlash, auth) = L.splitAt 2 bs
    (wwwDot, _)         = L.splitAt 4 bs
    (maybeAuth, rest)   = L.break (`elem` endAuthChars) bs

-- | Helper function for parseAuthority which always returns Url with authority.
parseAuthority' :: L.ByteString -> (Maybe L.ByteString, L.ByteString)
parseAuthority' bs
  | L.null maybeAuth || L.null rest = (Just bs, "")
  | otherwise                       = (Just maybeAuth, rest)
  where
    (maybeAuth, rest)   = L.break (`elem` endAuthChars) bs

--------------------------------------------------------------------------------
-- PARSING HELPER FUNCTIONS

-- | Normalises a path by removing dot-segments and references to index.html,
--   adding \'\/\' to the end of directories, and ignoring queries and
--   fragments.
--   Path /must be absolute/ (begins with \'\/\') or empty.
normalise :: L.ByteString    -- ^ Absolute path to be normalised, E.g.
                             --   "\/dir\/..\/index.html"
             -> L.ByteString -- ^ Returns normalised path, E.g. "\/"
normalise ""
  = "/"
normalise bs
  = foldl (flip L.append) "" $ normalise' bs []

-- | Helper function for normalise
normalise' :: L.ByteString -> [L.ByteString] -> [L.ByteString]
normalise' "" accum
  = accum
normalise' input accum
  -- REMOVE DOT SEGMENTS and INDEX.HTML
  | take2 == "//"           = normalise' (L.tail input) accum
  | take3 == "/.."          = normalise' (L.cons '/' drop3) $ drop 1 accum
  -- If it looks like a file, go back a directory
  | take2 == "/."           = if not (null accum) && isFile (head accum)
                              then normalise' (L.cons '/' drop2) $ tail accum
                              else normalise' (L.cons '/' drop2) accum
  | take11 == "/index.html" = normalise' (L.cons '/' drop11) accum
  -- REMOVE QUERIES and FRAGMENTS
  | L.take 1 input == "#"   = accum
  | L.take 1 input == "?"   = accum
  -- OTHERWISE CONTINUE
  | otherwise               = normalise' rest
                              $ L.cons '/' this : accum
  where
    (take2, drop2)   = L.splitAt 2 input
    (take3, drop3)   = L.splitAt 3 input
    (take11, drop11) = L.splitAt 11 input
    (this, rest)     = L.break (`elem` endAuthChars) $ if L.head input == '/'
                                                       then L.tail input
                                                       else input

-- | Returns whether a path segment 'looks like' it refers to a file.
--   A path segment looks like it refers to a file if it has a \'.\' character
--   (might mean there is a file extension).
--   */This function needs to be updated. May lead to undesirable results./*
isFile :: L.ByteString -> Bool
isFile
  = L.any (== '.')

-- | Characters which signify the end of the authority
endAuthChars :: String
endAuthChars
  = "/?#"

-- | Any characters not in the unreserved or reserved URL characters list are
--   percent encoded.
escape :: L.ByteString -- ^ A bytestring to encode
       -> L.ByteString --  ^ Returns with any unknown characters percent encoded
escape
  = L.foldr (L.append .percentEncode) ""

-- | Any character not in the unreserved or reserved URL characters list is
--   percent encoded.
percentEncode :: Char            -- ^ Character to encode
                 -> L.ByteString -- ^ Returns character, or percentage encoded
                                 --   character, if that character is not in the
                                 --   unreserved or reserved character sets
percentEncode c
  = if isValidChar c
    then L.pack [c]
    else L.pack $ '%' : showHex (ord c) ""

-- | Returns whether a character is in the unreserved or reserved character set
--   or not.
isValidChar :: Char    -- ^ Character to check
               -> Bool -- ^ Returns true if character is in the unreserved or
                       --   reserved character sets for URLs
isValidChar c
  = 'a' <= c && c <= 'z'
    || '0' <= c && c <= '9'
    || 'A' <= c && c <= 'Z'
    || c `elem` ("-_.~!*'();:@&=+$,/?#[]" :: String)

--------------------------------------------------------------------------------
-- FUNCTIONS to CHECK and SHOW URLS

-- | Returns true if two URLs have the same subdomain (authority).
isValidLinkedUrl :: Url     -- ^ The linked URL
                    -> Url  -- ^ The current URL to compare agains
                    -> Bool -- ^ Returns whether URLs have the same authority
isValidLinkedUrl linkedUrl thisUrl
  = authority linkedUrl == authority thisUrl

-- | Shows a Url object as a lazy bytestring.
showUrl :: Url             -- ^ Url object to be shown
           -> L.ByteString -- ^ Returns URL as lazy bytestring
showUrl (Url urlScheme urlAuth urlPath)
  = L.concat [urlScheme, "//", urlAuth, urlPath]
