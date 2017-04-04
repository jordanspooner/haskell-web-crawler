## HANDLE EDGE CASES
- *done* redirects
- *done* https
- *done* different subdomains
- *ignore* when redirects take you to a new subdomain
- *done* broken webpages (i.e. those that don't give 200 response) at start
- *done* broken webpages found whilst crawling
- *done* poorly formatted html (capitals, spaces, with/without quotes)
- *done* poorly formatted links (e.g. with slashes all over the place, www.)
- *done* fragment identifiers and relative urls
- *done* a links to files instead of webpages
- *done* mailto links
- *done* what to do for links within comments / scripts? - keep them?

## HTML PARSING - LINKS AND STATIC ASSETS
- *done* link, a: href
- *done* script, video, source, img, audio, iframe: src
- *done* object, embed: data

## TODO
- *done* request webpage
- *done* parse html, get static content and links in a user-defined data type
- *done* build json from this data type

## CLARITY AND TESTING
- *done* readme and commenting
- *done* tests

## WHY DID I DECIDE TO WRITE THIS IN HASKELL?
- it would have been much nicer in python!

## KNOWN BUGS AND ISSUEs
- cannot currently handle URLs with non-ASCII characters
- will follow redirects to subdomains (but will not continue from there)
