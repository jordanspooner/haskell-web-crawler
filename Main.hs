{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Utils.HtmlParser
import Utils.UrlParser
import Control.Exception
import Network.HTTP.Conduit
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L

websiteUrl :: String
websiteUrl
  = "http://www.schuller.it/"

main :: IO()
main = do
  let currentUrl = L.pack websiteUrl
  pages <- crawlWebsite [] [formatUrl currentUrl currentUrl]
  putStrLn ""
  putStrLn "Result:"
  L.putStr $ encodePretty pages

crawlWebsite :: [Webpage] -> [Url] -> IO [Webpage]
crawlWebsite seenPages []
  -- All viewable pages seen, return result
  = return seenPages
crawlWebsite seenPages urls@(currentUrl : _) = do
  -- Try to read current page
  maybeCurrentSource <- try $ simpleHttp $ L.unpack currentUrl
  case maybeCurrentSource of
    Left (_ :: HttpException) -> crawlFailure seenPages urls
    Right currentSource       -> crawlSuccess seenPages urls currentSource

crawlFailure :: [Webpage] -> [Url] -> IO [Webpage]
-- Pre: at least one url to read
crawlFailure _ []
  = error "Pre condition for crawlFailure not met"
crawlFailure seenPages (currentUrl : nextUrls) = do
  -- Page not reachable, continue to next url
  putStrLn ("WARNING: The page " ++ show currentUrl ++ " could not be reached.")
  crawlWebsite seenPages nextUrls

crawlSuccess :: [Webpage] -> [Url] -> L.ByteString -> IO [Webpage]
-- Pre: at least one url to read
crawlSuccess _ [] _
  = error "Pre condition for crawlSuccess not met"
crawlSuccess seenPages (currentUrl : nextUrls) currentSource = do
  -- Find all linked urls on current page
  putStrLn ("Crawling " ++ show currentUrl)
  let currentPage = crawlWebpage currentUrl currentSource
  -- Mark current page as seen and vist unseen urls from current page
  let nextSeenPages = currentPage : seenPages
  let newUrls = links currentPage \\ (map url nextSeenPages ++ nextUrls)
  crawlWebsite nextSeenPages $ nextUrls ++ newUrls
