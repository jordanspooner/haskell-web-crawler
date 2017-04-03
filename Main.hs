{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Utils.HtmlParser
import Utils.UrlParser
import Control.Exception
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO()
main = do
  putStrLn "Please provide a starting URL. \
  \Suggested format: \"http://www.example.com/\":"
  websiteUrl <- getLine
  pages <- crawlWebsite [] [] [parseUrl $ L.pack websiteUrl]
  putStrLn ""
  putStrLn "Result:"
  L.putStr $ encodePretty pages
  putStrLn ""

crawlWebsite :: [Webpage] -> [Url] -> [Url] -> IO [Webpage]
crawlWebsite seenPages _ []
  -- All viewable pages seen, return result
  = return seenPages
crawlWebsite seenPages seenUrls urls@(currentUrl : _) = do
  -- Try to read current page
  maybeCurrentSource <- try $ simpleHttp $ show currentUrl
  case maybeCurrentSource of
    Left (_ :: HttpException) -> crawlFailure seenPages seenUrls urls
    Right currentSource       -> crawlSuccess seenPages seenUrls urls
                                              currentSource

crawlFailure :: [Webpage] -> [Url] -> [Url] -> IO [Webpage]
-- Pre: at least one url to read
crawlFailure _ _ []
  = error "Pre condition for crawlFailure not met."
crawlFailure seenPages seenUrls (currentUrl : nextUrls) = do
  -- Page not reachable, continue to next url
  putStrLn ("WARNING: The page " ++ show currentUrl ++ " could not be reached.")
  crawlWebsite seenPages (currentUrl : seenUrls) nextUrls

crawlSuccess :: [Webpage] -> [Url] -> [Url] -> L.ByteString
                -> IO [Webpage]
-- Pre: at least one url to read
crawlSuccess _ _ [] _
  = error "Pre condition for crawlSuccess not met."
crawlSuccess seenPages seenUrls urls@(currentUrl : nextUrls) currentSource = do
  -- Find all linked urls on current page
  putStrLn ("Crawling " ++ show currentUrl)
  let currentPage = crawlWebpage currentUrl currentSource
  -- Mark current url as seen and vist unseen urls from current page
  let newUrls = links currentPage \\ (seenUrls ++ urls)
  crawlWebsite (currentPage : seenPages) (currentUrl : seenUrls)
    $ nextUrls ++ newUrls
