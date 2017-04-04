module Tests where

import           Tests.Main             (mainTests)
import           Tests.Utils.HtmlParser (htmlParserTests)
import           Tests.Utils.UrlParser  (urlParserTests)

main :: IO()
main = do
  mainTests
  htmlParserTests
  urlParserTests
