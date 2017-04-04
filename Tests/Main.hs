{-# LANGUAGE OverloadedStrings #-}

module Tests.Main where

import Main
import Utils.UrlParser
import Utils.HtmlParser
import Test.HUnit
import Data.List (sort)

--------------------------------------------------------------------------------
-- URLPARSER Tests

mainTests :: IO()
mainTests = do
  putStrLn ""
  putStrLn "** RUNNING TESTS FOR MAIN **"
  putStrLn ""
  putStrLn "* crawlSubdomain *"
  putStrLn ""
  putStrLn "** COMPLETED TESTS FOR MAIN **"
  putStrLn ""

--------------------------------------------------------------------------------
-- TEST SUBDOMAIN

expectedWebpageList1 :: [Webpage]
expectedWebpageList1
  = sort [ Webpage (Url "http:" "qiangfeng.co.uk" "/" expectedAssets1
                          expectedLinks1)
           , Webpage (Url "http:" "qiangfeng.co.uk" "/CV.pdf") [] []]

expectedAssets1 :: [Url]
expectedAssets1
  = [ Url "http:" "qiangfeng.co.uk" "/img/flappy-bird.png"
    , Url "http:" "qiangfeng.co.uk" "/img/ethereum.svg"
    , Url "http:" "qiangfeng.co.uk" "/css/global.css"
    , Url "http:" "qiangfeng.co.uk" "/css/normalise.css"
    , Url "https:" "fonts.googleapis.com" "/css"
    ]

expectedLinks1 :: [Url]
expectedLinks1
  = [Url "http:" "qiangfeng.co.uk" "/CV.pdf"]

--------------------------------------------------------------------------------

crawlSubdomainTests :: Test
crawlSubdomainTests
  = test [ "1" ~: do actualWebpageList1 <- sort $ crawlSubdomain [] []
                                          [parseUrl "qiangfeng.co.uk"]
                     assertEqual "1" actualWebpageList1 expectedWebpageList1 @? ""
         ]
