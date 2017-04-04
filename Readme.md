# Haskell Web Crawler

## Functionality

This is a command line program, that given a URL, returns a JSON which lists all the viewable pages on that subdomain, and for each page gives its URL and a list of any assets which it links to.

## Setup

This program requires GHC and several dependencies, which can be installed using Cabal. I suggest downloading and installing the Haskell Platform (if you don't already have it), which is available on most package installers. E.g. using `apt-get`, you can install the Haskell Platform with the command `sudo apt-get install haskell-platform`.

To install the dependencies, `cd` into the main directory and run the command `sudo cabal install --only-dependencies`.

## Usage

To run the program, you must then issue the command `runghc Main.hs`.

## Known Bugs (Features?) and Issues

- The program cannot currently handle non-ASCII characters in URLs.
- The program considers a linked page as anything within a HTML "a" tag. This may include documents, images, etc. in addition to other webpages.
- When parsing HTML, the program will consider *everything*: this includes commented code and code that is within scripts.
- If the crawler finds a link (to a page on the current subdomain) that redirects, it will follow this link and crawl the redirected page, including if that page is actually on a different subdomain. Note however that it will not continue to crawl on a different subdomain.
