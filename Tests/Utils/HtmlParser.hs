{-# LANGUAGE OverloadedStrings #-}

module Tests.Utils.HtmlParser where

import           Data.List                  (sort)

import qualified Data.ByteString.Lazy.Char8 as L
import           Test.HUnit

import           Utils.HtmlParser
import           Utils.UrlParser

--------------------------------------------------------------------------------
-- HTMLPARSER TESTS

htmlParserTests :: IO()
htmlParserTests = do
  putStrLn ""
  putStrLn "** RUNNING TESTS FOR UTILS.HTMLPARSER **"
  putStrLn ""
  putStrLn "* crawlWebpage *"
  _ <- runTestTT crawlWebpageTests
  putStrLn "* parseHtml *"
  _ <- runTestTT parseHtmlTests
  putStrLn "* parseAttributes *"
  _ <- runTestTT parseAttributesTests
  putStrLn "* parseName *"
  _ <- runTestTT parseNameTests
  putStrLn "* parseValue *"
  _ <- runTestTT parseValueTests
  putStrLn ""
  putStrLn "** COMPLETED TESTS FOR UTILS.HTMLPARSER **"
  putStrLn ""

--------------------------------------------------------------------------------
-- TEST HTML

testUrl :: Url
testUrl = Url "http:" "jordanspooner.com" "/page/"

assetUrlBs1, assetUrlBs2, assetUrlBs3, assetUrlBs4 :: L.ByteString

assetUrlBs1 = "../css/non-existent.css"
assetUrlBs2 = "javascript.js"
assetUrlBs3 = "HTTP://S.Hswstatic.Com/gif/cloud-1.jpg"
assetUrlBs4 = "/img/image.png"

assetUrl1, assetUrl2, assetUrl3, assetUrl4 :: Url

assetUrl1 = Url "http:" "jordanspooner.com" "/css/non-existent.css"
assetUrl2 = Url "http:" "jordanspooner.com" "/page/javascript.js"
assetUrl3 = Url "http:" "s.hswstatic.com" "/gif/cloud-1.jpg"
assetUrl4 = Url "http:" "jordanspooner.com" "/img/image.png"

linkedUrlBs1, linkedUrlBs2, linkedUrlBs3, linkedUrlBs4, linkedUrlBs5,
  linkedUrlBs6, linkedUrlBs7, linkedUrlBs8, linkedUrlBs9 :: L.ByteString

linkedUrlBs1 = "http://jordanspooner.com/page/"
linkedUrlBs2 = "//jordanspooner.com/about/index.html"
linkedUrlBs3 = "test"
linkedUrlBs4 = "page.html"
linkedUrlBs5 = "/other"
linkedUrlBs6 = ".."
linkedUrlBs7 = "www.google.com"                        -- Invalid link
linkedUrlBs8 = "mailto:jordan@jordanspooner.com"       -- Invalid link
linkedUrlBs9 = "https://portal.jordanspooner.com/123/" -- Invalid link

linkedUrl1, linkedUrl2, linkedUrl3, linkedUrl4, linkedUrl5, linkedUrl6,
  linkedUrl7, linkedUrl8, linkedUrl9 :: Url

linkedUrl1 = Url "http:" "jordanspooner.com" "/page/"
linkedUrl2 = Url "http:" "jordanspooner.com" "/about/"
linkedUrl3 = Url "http:" "jordanspooner.com" "/page/test"
linkedUrl4 = Url "http:" "jordanspooner.com" "/page/page.html"
linkedUrl5 = Url "http:" "jordanspooner.com" "/other"
linkedUrl6 = Url "http:" "jordanspooner.com" "/"
linkedUrl7 = Url "http:" "www.google.com" "/"               -- Invalid link
linkedUrl8 = Url "http:" "/" "/"                            -- Invalid link
linkedUrl9 = Url "http:" "portal.jordanspooner.com" "/123/" -- Invalid link

testHtml :: L.ByteString
testHtml
  = "<HTML>\n\n<HEAD>\n\n<TITLE>Page Title</TITLE>\n   <link rel=\"stylesheet\"\
    \ type=\"text/css\" href='../css/non-existent.css'>\n    <SCRIPT   src='jav\
    \ascript.js'></script>\n</HEAD>\n\n<BODY BGCOLOR=\"FFFFFF\">\n\n<CENTER><IM\
    \G   SrC=\"HTTP://S.Hswstatic.Com/gif/cloud-1.jpg\" align=\"BOTTOM\"> </CEN\
    \TER>\n\n<HR>\n\n<A    href=    http://jordanspooner.com/page/ >This</a>\n \
    \\nis the page you are on.\n\n<p>Another <A    href=    '//jordanspooner.co\
    \m/about/index.html' >link</a>. </p>\n<p>Another <A    href=    test >link<\
    \/a>. </p>\n<p>Another <A href=page.html >link</a>. </p>\n<p>     This   is\
    \ another <A href=/other >link</a>. </p>\n<p>Another <A    href=.. >link</a\
    \>. </p>\n<p>Another <A    href= 'www.google.com' >link</a>. </p>\n\n<H1>Th\
    \is is a Header</H1>\n\n<H2>This is a Medium Header</H2>\n\nSend me mail at\
    \ <a href=\"mailto:jordan@jordanspooner.com\">\n\njordan@jordanspooner.com<\
    \/A>.\n\n<P> This is a new paragraph!\n\n<P> <B>This is a new paragraph!</B\
    \> \n\n<BR /> <B><I>This is a new <a HRef=\"https://portal.jordanspooner.co\
    \m/123/\"> sentence</a> without a paragraph break, in bold italics.</I></B>\
    \\n\n<imG src=\"/img/image.png\" \"this text shouldn't be here\">\n\n<HR>\n\
    \\n</BODY>\n\n</HTML>\n"

--------------------------------------------------------------------------------
-- PARSING FUNCTIONS TESTS

crawlWebpageTests :: Test
crawlWebpageTests
  = test [ "crawl" ~: Webpage actualUrl (sort actualAssets) (sort actualLinks)
             ~?= Webpage testUrl
                         (sort [assetUrl1, assetUrl2, assetUrl3, assetUrl4])
                         (sort [linkedUrl1, linkedUrl2, linkedUrl3, linkedUrl4,
                                linkedUrl5, linkedUrl6])
         ]
  where
    Webpage actualUrl actualAssets actualLinks = crawlWebpage testUrl testHtml

parseHtmlTests :: Test
parseHtmlTests
  = test [ "html" ~: (sort actualAssets, sort actualLinks)
             ~?= (sort [Just assetUrlBs1, Just assetUrlBs2, Just assetUrlBs3,
                        Just assetUrlBs4],
                  sort [Just linkedUrlBs1, Just linkedUrlBs2, Just linkedUrlBs3,
                        Just linkedUrlBs4, Just linkedUrlBs5, Just linkedUrlBs6,
                        Just linkedUrlBs7, Just linkedUrlBs8,
                        Just linkedUrlBs9])
         ]
  where
    (actualAssets, actualLinks) = parseHtml testHtml [] []

parseAttributesTests :: Test
parseAttributesTests
  = test [ "double"  ~: parseAttributes "href=\"link\">rest"
             ~?= (Just "link", ">rest")
         , "single"  ~: parseAttributes "href=\'link\'>rest"
             ~?= (Just "link", ">rest")
         , "noquote" ~: parseAttributes "href=link>rest"
             ~?= (Just "link", ">rest")
         , "spaces"  ~: parseAttributes "    href   = link   >rest"
             ~?= (Just "link", ">rest")
         , "noatts"  ~: parseAttributes ">rest"
             ~?= (Nothing, ">rest")
         , "noclose" ~: parseAttributes "href = link  rest"
             ~?= (Just "link", "rest")
         , "second"  ~: parseAttributes "otheratt = something href = link>rest"
             ~?= (Just "link", ">rest")
         ]

parseNameTests :: Test
parseNameTests
  = test [ "normal"   ~: parseName "href=rest"
             ~?= ("href", "=rest")
         , "spaces"   ~: parseName "href  =   rest"
             ~?= ("href", "  =   rest")
         , "noequals" ~: parseName "href rest"
             ~?= ("href", " rest")
         ]

parseValueTests :: Test
parseValueTests
  = test [ "value" ~: parseValue "link rest"
             ~?= ("link", " rest")
         ]
