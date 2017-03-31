## HANDLE EDGE CASES
- *done* redirects
- *done - but need to check link at beginning* https
- *done* different subdomains
- broken webpages (i.e. those that don't give 200 response) at start
- *done* broken webpages found whilst crawling
- poorly formatted html (capitals, spaces, with/without quotes)
- poorly formatted links (e.g. with slashes all over the place, www.)
- fragment identifiers and relative urls
- a links to files instead of webpages
- *done* mailto links
- what to do for links within comments / scripts?
- robots.txt

## HTML PARSING - LINKS AND STATIC ASSETS
- *done* link, a: href
- *done* script, video, source, img, audio, iframe: src
- *done* object, embed: data

## TODO
- *done* request webpage
- *done* parse html, get static content and links in a user-defined data type
- build json from this data type

## CLARITY AND TESTING
- readme and commenting
- tests

## EXTENSIONS
- check sitemap.xml first
- respect robots.txt

## FURTHER EXTENSIONS
- improve haskell style (folds etc.)
- try also writing in python / java
