{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception          (try)
import           Data.List                  ((\\))

import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L
import           Network.HTTP.Conduit       (HttpException, simpleHttp)

import           Utils.HtmlParser
import           Utils.UrlParser

--------------------------------------------------------------------------------

-- | Given URL as user input, crawls and pretty prints JSON of subdomain
--   structure.
--   Input can be given with or without scheme. If no scheme is provided, http
--   will be selected by default.
--   Output JSON is a list of viewable webpages, each with its URL and a list
--   of any assets.
--   Example:
--   
--   >>> main
--   Please provide a starting URL:
--   >>> www.haskell.org
--   [
--   {
--       "url": "http://www.haskell.org/",
--       "assets": [
--           "http://www.haskell.org/static/js/tryhaskell.pages.js",
--           "http://www.haskell.org/static/js/tryhaskell.js",
--           "http://www.haskell.org/static/js/jquery.console.js",
--           "http://www.haskell.org/static/js/home.js",
--           "http://www.haskell.org/static/js/bootstrap.min.js",
--           "http://www.haskell.org/static/js/jquery.js",
--           "http://www.haskell.org/static/img/rackspace.svg",
--           "https://i.vimeocdn.com/video/452269027_150x84.jpg",
--           "https://i.vimeocdn.com/video/456929840_150x84.jpg",
--           "https://i.vimeocdn.com/video/456929997_150x84.jpg",
--           "https://i.vimeocdn.com/video/469227196_150x84.jpg",
--           "https://i.vimeocdn.com/video/469235326_150x84.jpg",
--           "https://i.vimeocdn.com/video/476988542_150x84.jpg",
--           "http://www.haskell.org/static/img/haskell-logo.svg",
--           "http://www.haskell.org/static/css/hl.min.css",
--           "https://fonts.googleapis.com/css",
--           "http://www.haskell.org/static/img/favicon.ico"
--       ]
--   },
--   ... (all reachable pages)
--   ]
main :: IO()
main = do
  putStrLn "Please provide a starting URL:"
  websiteUrl <- getLine
  pages <- crawlSubdomain [] [] [parseUrl $ L.pack websiteUrl]
  putStrLn ""
  putStrLn "Result:"
  L.putStr $ encodePretty pages
  putStrLn ""

-- | Accumulates a list of unique visited webpages and URLs by crawling a given
--   list of URLs.
--   Prints update for each page crawled and for each page not found.
--   Accumulators should usually be initiated as empty lists.
crawlSubdomain :: [Webpage]       -- ^ The accumulator for visited webpages
                  -> [Url]        -- ^ The accumulator for visited URLs
                  -> [Url]        -- ^ The list of URLs to still visit
                  -> IO [Webpage] -- ^ Returns final list of visited webpages
crawlSubdomain seenPages _ []
  -- All viewable pages seen, return result
  = return seenPages
crawlSubdomain seenPages seenUrls urls@(currentUrl : _) = do
  -- Try to read current page
  maybeCurrentSource <- try $ simpleHttp $ show currentUrl
  case maybeCurrentSource of
    Left (_ :: HttpException) -> crawlFailure seenPages seenUrls urls
    Right currentSource       -> crawlSuccess seenPages seenUrls urls
                                              currentSource

-- | Outputs a warning message with URL not reached and crawls remaining URLs
--   with this URL marked as seen.
--   There *must be at least one URL to still visit*.
crawlFailure :: [Webpage]       -- ^ The list of visited webpages
                -> [Url]        -- ^ The list of visited URLs
                -> [Url]        -- ^ The list of this URL and URLs to still
                                --   visit
                -> IO [Webpage] -- ^ Returns final list of visited webpages
crawlFailure _ _ []
  = error "Pre condition for crawlFailure not met."
crawlFailure seenPages seenUrls (currentUrl : nextUrls) = do
  -- Page not reachable, continue to next URL
  putStrLn ("WARNING: The page " ++ show currentUrl ++ " could not be reached.")
  crawlSubdomain seenPages (currentUrl : seenUrls) nextUrls

-- | Outputs message with crawled URL, crawls and saves Webpage, marks URL as
--   seen and crawls remaining URLs, including any URLs marked on this page.
--   There *must be at least one URL to still visit*.
crawlSuccess :: [Webpage]       -- ^ The list of visited webpages
                -> [Url]        -- ^ The list of visited URLs
                -> [Url]        -- ^ The list of this URL and URLs to still
                                --   visit
                -> L.ByteString -- ^ The source at this URL
                -> IO [Webpage] -- ^ Returns final list of visited webpages
crawlSuccess _ _ [] _
  = error "Pre condition for crawlSuccess not met."
crawlSuccess seenPages seenUrls urls@(currentUrl : nextUrls) currentSource = do
  -- Find all linked urls on current page
  putStrLn ("Crawling " ++ show currentUrl)
  let currentPage = crawlWebpage currentUrl currentSource
  -- Mark current url as seen and vist unseen urls from current page
  let newUrls = links currentPage \\ (seenUrls ++ urls)
  crawlSubdomain (currentPage : seenPages) (currentUrl : seenUrls)
    $ nextUrls ++ newUrls
