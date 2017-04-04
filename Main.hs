{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Utils.HtmlParser
import Utils.UrlParser
import Control.Exception
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L

-- | Given URL as user input, crawls and pretty prints JSON of subdomain
--   structure.
main :: IO()
main = do
  putStrLn "Please provide a starting URL. \
  \Suggested format: \"http://www.example.com/\":"
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
  -- Page not reachable, continue to next url
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
