{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.ParallelIO (parallel)
import           Control.Exception             (try)
import           Data.IORef
import           Data.List                     ((\\))

import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.ByteString.Lazy.Char8    as L
import           Network.HTTP.Conduit          (HttpException, simpleHttp)

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
  inputUrl <- getLine
  let currentUrl = parseUrl $ L.pack inputUrl
  seenUrls <- newIORef [currentUrl]
  pages <- crawlSubdomain currentUrl seenUrls
  putStrLn ""
  putStrLn "Result:"
  L.putStr $ encodePretty pages
  putStrLn ""

--------------------------------------------------------------------------------

-- | Crawls the subdomain of the given URL, returning a list of Webpages of all
--   the reachable pages from the given URL, excluding any which are at URLs in
--   the given list of seen URLs.
crawlSubdomain :: Url             -- ^ The URL to start crawling from
                  -> IORef [Url]  -- ^ A list of seen URLs to be excluded
                  -> IO [Webpage] -- ^ Returns a list of all reachable Webpages
crawlSubdomain currentUrl seenUrls = do
  maybeCurrentSource <- try $ simpleHttp $ show currentUrl
  case maybeCurrentSource of
    Left (_ :: HttpException) -> crawlFailure currentUrl
    Right currentSource       -> crawlSuccess currentSource currentUrl seenUrls

-- | Outputs a warning message with the URL that could not be reached. Returns
--   an empty list of Webpages.
crawlFailure :: Url             -- ^ The URL that could not be reached.
                -> IO [Webpage] -- ^ Returns an empty list.
crawlFailure currentUrl = do
  putStrLn ("WARNING: The page " ++ show currentUrl ++ " could not be reached.")
  return []

-- | Outputs a message with the URL being crawled. Crawls the webpage, updates
--   the seen URLs with any unseen URLs present on that page, and concurrently
--   crawls the unseen URLs. Returns all reachable pages that haven't been seen
--   yet.
crawlSuccess :: L.ByteString    -- ^ The HTML source of the given URL
                -> Url          -- ^ The URL being crawled
                -> IORef [Url]  -- ^ A list of seen URLs to be excluded
                -> IO [Webpage] -- ^ Returns a list of all reachable Webpages
crawlSuccess currentSource currentUrl seenUrls = do
  putStrLn ("Crawling " ++ show currentUrl)
  let currentPage = crawlWebpage currentUrl currentSource
  newUrls <- atomicModifyIORef' seenUrls (updateUrls $ links currentPage)
  nextPages <- parallel $ map (`crawlSubdomain` seenUrls) newUrls
  return (currentPage : concat nextPages)

-- | Given a list of URLs, produces a tuple conataining an updated list of seen
--   URLs and a list of any unseen URLs.
updateUrls :: [Url]             -- ^ A list of crawled URLs to be processed
              -> [Url]          -- ^ The list of URLs seen so far
              -> ([Url], [Url]) -- ^ Returns a tuple containing the new list of
                                --   seen URLs, and a list of unseen URLs
updateUrls currentPageUrls seenUrls
  = (unseenUrls ++ seenUrls, unseenUrls)
  where
    unseenUrls = currentPageUrls \\ seenUrls
