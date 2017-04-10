{-# LANGUAGE OverloadedStrings #-}

module Tests.Utils.UrlParser where

import           Data.List                  (sort)

import qualified Data.ByteString.Lazy.Char8 as L
import           Test.HUnit

import           Utils.UrlParser

--------------------------------------------------------------------------------
-- URLPARSER TESTS

urlParserTests :: IO()
urlParserTests = do
  putStrLn ""
  putStrLn "** RUNNING TESTS FOR UTILS.URLPARSER **"
  putStrLn ""
  putStrLn "* formatMaybeUrls *"
  _ <- runTestTT formatMaybeUrlsTests
  putStrLn "* parseUrl *"
  _ <- runTestTT parseUrlTests
  putStrLn "* parseUrlRelativeTo *"
  _ <- runTestTT parseUrlRelativeToTests
  putStrLn "* parseScheme *"
  _ <- runTestTT parseSchemeTests
  putStrLn "* parseAuthority *"
  _ <- runTestTT parseAuthorityTests
  putStrLn "* normalise *"
  _ <- runTestTT normaliseTests
  putStrLn "* isValidLinkedUrl *"
  _ <- runTestTT isValidLinkedUrlTests
  putStrLn "* showUrl *"
  _ <- runTestTT showUrlTests
  putStrLn ""
  putStrLn "** COMPLETED TESTS FOR UTILS.URLPARSER **"
  putStrLn ""

--------------------------------------------------------------------------------
-- TEST URL BYTESTRINGS

urlBs1, urlBs2, urlBs3, urlBs4, urlBs5, urlBs6, urlBs7, urlBs8, urlBs9, urlBs10,
  urlBs11, urlBs12, urlBs13, urlBs14, urlBs15, urlBs16, urlBs17, urlBs18,
  urlBs19, urlBs20, urlBs21, urlBs22, urlBs23, urlBs24, urlBs25, urlBs26,
  urlBs27, urlBs28, urlBs29, urlBs30, urlBs31, urlBs32, urlBs33, urlBs34
  :: L.ByteString

urlBs1 = "http://www.example.com"
urlBs2 = "https://www.example.com"
urlBs3 = "example.com"
urlBs4 = "www.example.com"
urlBs5 = "www.example.com/"
urlBs6 = "//www.example.com/"

urlBs7  = "www.example.com/index.html"
urlBs8  = "www.example.com/page.html"
urlBs9  = "www.example.com/dir1/dir2/dir3/index.html"
urlBs10 = "www.example.com/dir1/dir2/page.html"

urlBs11 = "www.example.com/."
urlBs12 = "www.example.com/././././."
urlBs13 = "www.example.com/dir1/.."
urlBs14 = "www.example.com/dir1/./dir2/./../.././dir1/./dir2/dir3/dir4/.."

urlBs15 = "HTTP://WWW.EXAMPLE.COM"
urlBs16 = "HTTPS://WWW.EXAMPLE.COM"

urlBs17 = "mailto:person@example.com"
urlBs18 = "javascript:function"

urlBs19 = "www.example.com/page.html?query"
urlBs20 = "www.example.com/page.html#fragment"

urlBs21 = "dir1/"
urlBs22 = "page.html"
urlBs23 = "dir4/index.html"
urlBs24 = "."
urlBs25 = "/."
urlBs26 = ".."
urlBs27 = "/.."
urlBs28 = "../page.html"
urlBs29 = "/../page.html"

urlBs30 = "http://www.google.com/"
urlBs31 = "https://www.google.com/dir1/dir2/dir3/dir4/"
urlBs32 = "http://mail.google.com/"

urlBs33 = "http://example.com/décembre.html"
urlBs34 = "http://example.com/dir"

--------------------------------------------------------------------------------
-- TEST URLS

url1, url2, url3, url4, url5, url6, url7, url8, url9, url10, url11, url12,
  url13, url14, url15, url16, url17, url18, url19, url20 :: Url

url4', url17', url18', url21', url22', url23', url24', url25', url26', url27',
  url28', url29' :: Url

url30, url31, url32 :: Url

url33, url34, url33', url33'', url34' :: Url

url1  = Url "http:" "www.example.com" "/"
url2  = Url "https:" "www.example.com" "/"
url3  = Url "http:" "example.com" "/"
url4  = url1
url5  = url1
url6  = url1
url4' = url2   -- Relative to url2

url7  = url1
url8  = Url "http:" "www.example.com" "/page.html"
url9  = Url "http:" "www.example.com" "/dir1/dir2/dir3/"
url10 = Url "http:" "www.example.com" "/dir1/dir2/page.html"

url11 = url1
url12 = url1
url13 = url1
url14 = url9

url15 = url1
url16 = url2

url17  = Url "http:" "" "/"
url18  = Url "http:" "" "/"
url17' = url9  -- Relative to url9
url18' = url9  -- Relative to url9

url19  = url8
url20  = url8

url21' = Url "http:" "www.example.com" "/dir1/"
               -- Relative to url1
url22' = url8  -- Relative to url1
url23' = Url "http:" "www.example.com" "/dir1/dir2/dir3/dir4/"
               -- Relative to url9
url24' = url9  -- Relative to url9
url25' = url1  -- Relative to url9
url26' = url9  -- Relative to url23'
url27' = url1  -- Relative to url1
url28' = url10 -- Relative to url9
url29' = url8  -- Relative to url9

url30 = Url "http:" "www.google.com" "/"
url31 = Url "https:" "www.google.com" "/dir1/dir2/dir3/dir4/"
url32 = Url "http:" "mail.google.com" "/"

url33   = Url "http:" "example.com" "/d\233cembre.html"
url33'  = url8 -- urlBs8 relative to url33
url33'' = url3 -- urlBs24 relative to url33
url34   = Url "http:" "example.com" "/dir"
url34'  = Url "http:" "example.com" "/dir/page.html"
               -- urlBs22 relative to url34
url34'' = Url "http:" "example.com" "/page.html"
               -- urlBs28 relative to url34

--------------------------------------------------------------------------------
-- FUNCTIONS to PARSE URL LISTS TESTS

formatMaybeUrlsTests :: Test
formatMaybeUrlsTests
  = test [ "assets" ~: sort (formatMaybeAssetUrls
             [Just urlBs1, Just urlBs2, Just urlBs8, Just urlBs9, Just urlBs17,
              Just urlBs19, Just urlBs20, Just urlBs23, Just urlBs24,
              Just urlBs25, Just urlBs28, Just urlBs29, Just urlBs30,
              Just urlBs31, Just urlBs32, Nothing, Nothing] url9)
              ~?= sort [url1, url8, url9, url10, url23', url30, url31, url32]
         , "links" ~: sort (formatMaybeLinkedUrls
             [Just urlBs1, Just urlBs2, Just urlBs8, Just urlBs9, Just urlBs17,
              Just urlBs19, Just urlBs20, Just urlBs23, Just urlBs24,
              Just urlBs25, Just urlBs28, Just urlBs29, Just urlBs30,
              Just urlBs31, Just urlBs32, Nothing, Nothing] url9)
             ~?= sort [url1, url8, url9, url10, url23']
         ]

--------------------------------------------------------------------------------
-- URL PARSING FUNCTIONS TESTS

parseUrlTests :: Test
parseUrlTests
  = test [ "url1"  ~: parseUrl urlBs1  ~?= url1
         , "url2"  ~: parseUrl urlBs2  ~?= url2
         , "url3"  ~: parseUrl urlBs3  ~?= url3
         , "url4"  ~: parseUrl urlBs4  ~?= url4
         , "url5"  ~: parseUrl urlBs5  ~?= url5
         , "url6"  ~: parseUrl urlBs6  ~?= url6
         , "url7"  ~: parseUrl urlBs7  ~?= url7
         , "url8"  ~: parseUrl urlBs8  ~?= url8
         , "url9"  ~: parseUrl urlBs9  ~?= url9
         , "url10" ~: parseUrl urlBs10 ~?= url10
         , "url11" ~: parseUrl urlBs11 ~?= url11
         , "url12" ~: parseUrl urlBs12 ~?= url12
         , "url13" ~: parseUrl urlBs13 ~?= url13
         , "url14" ~: parseUrl urlBs14 ~?= url14
         , "url15" ~: parseUrl urlBs15 ~?= url15
         , "url16" ~: parseUrl urlBs16 ~?= url16
         , "url17" ~: parseUrl urlBs17 ~?= url17
         , "url18" ~: parseUrl urlBs18 ~?= url18
         , "url19" ~: parseUrl urlBs19 ~?= url19
         , "url20" ~: parseUrl urlBs20 ~?= url20
         , "url30" ~: parseUrl urlBs30 ~?= url30
         , "url31" ~: parseUrl urlBs31 ~?= url31
         , "url32" ~: parseUrl urlBs32 ~?= url32
         , "url33" ~: parseUrl urlBs33 ~?= url33
         , "url34" ~: parseUrl urlBs34 ~?= url34
         ]

parseUrlRelativeToTests :: Test
parseUrlRelativeToTests
  = test [ "url4'"   ~: parseUrlRelativeTo urlBs4 url2    ~?= url4'
         , "url17'"  ~: parseUrlRelativeTo urlBs17 url9   ~?= url17'
         , "url18'"  ~: parseUrlRelativeTo urlBs18 url9   ~?= url18'
         , "url21'"  ~: parseUrlRelativeTo urlBs21 url1   ~?= url21'
         , "url22'"  ~: parseUrlRelativeTo urlBs22 url1   ~?= url22'
         , "url23'"  ~: parseUrlRelativeTo urlBs23 url9   ~?= url23'
         , "url24'"  ~: parseUrlRelativeTo urlBs24 url9   ~?= url24'
         , "url25'"  ~: parseUrlRelativeTo urlBs25 url9   ~?= url25'
         , "url26'"  ~: parseUrlRelativeTo urlBs26 url23' ~?= url26'
         , "url27'"  ~: parseUrlRelativeTo urlBs27 url1   ~?= url27'
         , "url28'"  ~: parseUrlRelativeTo urlBs28 url9   ~?= url28'
         , "url28'"  ~: parseUrlRelativeTo urlBs29 url9   ~?= url29'
         , "url33'"  ~: parseUrlRelativeTo urlBs8 url33   ~?= url33'
         , "url33''" ~: parseUrlRelativeTo urlBs24 url33  ~?= url33''
         , "url34'"  ~: parseUrlRelativeTo urlBs22 url34  ~?= url34'
         , "url34''" ~: parseUrlRelativeTo urlBs28 url34  ~?= url34''
         ]

parseSchemeTests :: Test
parseSchemeTests
 = test [ "http:"   ~: parseScheme "http:rest"   ~?= (Just "http:", "rest")
        , "https:'" ~: parseScheme "https:rest"  ~?= (Just "https:", "rest")
        , "mailto:" ~: parseScheme "mailto:rest" ~?= (Nothing, "")
        , "none'"   ~: parseScheme "rest"        ~?= (Nothing, "rest")
        ]

parseAuthorityTests :: Test
parseAuthorityTests
 = test [ "//"       ~: parseAuthority "//eg.com"
            ~?= (Just "eg.com", "")
        , "www."     ~: parseAuthority "www.eg.com"
            ~?= (Just "www.eg.com", "")
        , "www./"    ~: parseAuthority "www.eg.com/rest"
            ~?= (Just "www.eg.com", "/rest")
        , "dir"      ~: parseAuthority "dir/"
            ~?= (Nothing, "dir/")
        , "/dir"     ~: parseAuthority "/dir/"
            ~?= (Nothing, "/dir/")
        , "page"     ~: parseAuthority "page.html"
            ~?= (Nothing, "page.html")
        , "dir/page" ~: parseAuthority "dir/page.html"
            ~?= (Nothing, "dir/page.html")
        , "./"       ~: parseAuthority "./rest"
            ~?= (Nothing, "./rest")
        , "../"      ~: parseAuthority "../rest"
            ~?= (Nothing, "../rest")
        ]

--------------------------------------------------------------------------------
-- PARSING HELPER FUNCTIONS TESTS

normaliseTests :: Test
normaliseTests
 = test [ "empty"     ~: normalise ""                ~?= "/"
        , "./"        ~: normalise "/././1/"         ~?= "/1/"
        , "../"       ~: normalise "/../../1/"       ~?= "/1/"
        , "index"     ~: normalise "/index.html"     ~?= "/"
        , "dir/."     ~: normalise "/1/././2/././3/" ~?= "/1/2/3/"
        , "dir/.."    ~: normalise "/1/2/3/../../2/" ~?= "/1/2/"
        , "dir/index" ~: normalise "/1/2/index.html" ~?= "/1/2/"
        , "dir"       ~: normalise "/1/2/3"          ~?= "/1/2/3"
        , "dir/page"  ~: normalise "/1/2/page.html"  ~?= "/1/2/page.html"
        , "?"         ~: normalise "/page?rest"      ~?= "/page"
        , "#"         ~: normalise "/page#rest"      ~?= "/page"
        ]

--------------------------------------------------------------------------------
-- FUNCTIONS to CHECK and SHOW URLS TESTS

isValidLinkedUrlTests :: Test
isValidLinkedUrlTests
 = test [ "True"  ~: isValidLinkedUrl url30 url31 ~?= True
        , "False" ~: isValidLinkedUrl url30 url32 ~?= False
        ]

showUrlTests :: Test
showUrlTests
 = test [ "url30" ~: showUrl url30
            ~?= "http://www.google.com/"
        , "url31" ~: showUrl url31
            ~?= "https://www.google.com/dir1/dir2/dir3/dir4/"
        , "url32" ~: showUrl url32
            ~?= "http://mail.google.com/"
        ]
