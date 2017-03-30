module Main where

import Network.HTTP.Conduit
import Utils.HtmlParser
import Utils.JsonBuilder

urlToCrawl :: String
urlToCrawl
  = "https://www.doc.ic.ac.uk/~js4416"

main :: IO()
main = do
  site <- simpleHttp urlToCrawl
  print $ crawlWebpage urlToCrawl site
