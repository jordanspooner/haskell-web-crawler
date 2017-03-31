{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Utils.HtmlParser
import Utils.UrlFormatter
import Utils.JsonBuilder
import Control.Exception
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L

websiteUrl :: String
websiteUrl
  = "http://www.schuller.it/"

main :: IO()
main = do
  let currentUrl = L.pack websiteUrl
  pages <- crawlWebsite [] [formatUrl currentUrl currentUrl]
  print pages

crawlWebsite :: [Webpage] -> [Url] -> IO [Webpage]
crawlWebsite seenPages []
  -- All viewable pages seen, return result
  = return seenPages

crawlWebsite seenPages urls@(currentUrl : nextUrls) = do
  -- Try to read current page
  maybeCurrentSource <- try $ simpleHttp $ L.unpack currentUrl
  print ("Crawling " ++ show currentUrl)
  case maybeCurrentSource of
    -- Page not reachable, continue to next url
    Left (_ :: HttpException) -> crawlWebsite seenPages nextUrls
    -- Otherwise read page
    Right currentSource       -> crawlNext seenPages urls currentSource

crawlNext :: [Webpage] -> [Url] -> L.ByteString -> IO [Webpage]
-- Pre: at least one url to read
crawlNext seenPages (currentUrl : nextUrls) currentSource = do
  -- Find all linked urls on current page
  let currentPage = crawlWebpage currentUrl currentSource
  -- Mark current page as seen and vist unseen urls from current page
  let nextSeenPages = currentPage : seenPages
  let newUrls = links currentPage \\ (map url nextSeenPages ++ nextUrls)
  crawlWebsite nextSeenPages $ nextUrls ++ newUrls
