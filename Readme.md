# Haskell Web Crawler

## Functionality

This is a command line program, that given a URL, returns a JSON which lists all the viewable pages on that subdomain, and for each page gives its URL and a list of any assets which it links to.

## Example

Given the input `www.haskell.org`, the program should return a JSON formatted string similar to the one below:

```
[
{
    "url": "http://www.haskell.org/",
    "assets": [
        "http://www.haskell.org/static/js/tryhaskell.pages.js",
        "http://www.haskell.org/static/js/tryhaskell.js",
        "http://www.haskell.org/static/js/jquery.console.js",
        "http://www.haskell.org/static/js/home.js",
        "http://www.haskell.org/static/js/bootstrap.min.js",
        "http://www.haskell.org/static/js/jquery.js",
        "http://www.haskell.org/static/img/rackspace.svg",
        "https://i.vimeocdn.com/video/452269027_150x84.jpg",
        "https://i.vimeocdn.com/video/456929840_150x84.jpg",
        "https://i.vimeocdn.com/video/456929997_150x84.jpg",
        "https://i.vimeocdn.com/video/469227196_150x84.jpg",
        "https://i.vimeocdn.com/video/469235326_150x84.jpg",
        "https://i.vimeocdn.com/video/476988542_150x84.jpg",
        "http://www.haskell.org/static/img/haskell-logo.svg",
        "http://www.haskell.org/static/css/hl.min.css",
        "https://fonts.googleapis.com/css",
        "http://www.haskell.org/static/img/favicon.ico"
    ]
},
{
    "url": "http://www.haskell.org/news/",
    "assets": [
        "http://www.haskell.org/static/js/home.js",
        "http://www.haskell.org/static/js/bootstrap.min.js",
        "http://www.haskell.org/static/js/jquery.js",
        "http://www.haskell.org/static/img/rackspace.svg",
        "http://www.haskell.org/static/img/haskell-logo.svg",
        "http://www.haskell.org/static/css/hl.min.css",
        "https://fonts.googleapis.com/css",
        "http://www.haskell.org/static/img/favicon.ico"
    ]
},
{
    "url": "http://www.haskell.org/documentation/",
    "assets": [
        "http://www.haskell.org/static/js/home.js",
        "http://www.haskell.org/static/js/bootstrap.min.js",
        "http://www.haskell.org/static/js/jquery.js",
        "http://www.haskell.org/static/img/rackspace.svg",
        "http://www.haskell.org/static/img/haskell-logo.svg",
        "http://www.haskell.org/static/css/hl.min.css",
        "https://fonts.googleapis.com/css",
        "http://www.haskell.org/static/img/favicon.ico"
    ]
},
...
]
```

## Setup

This program requires GHC and several dependencies, which can be installed using Cabal. I suggest downloading and installing the Haskell Platform (if you don't already have it), which is available on most package installers. E.g. using `apt-get`, you can install the Haskell Platform with the command `sudo apt-get install haskell-platform`.

To install the dependencies, `cd` into the main directory and run the command `sudo cabal install --only-dependencies`.

## Usage

To run the program, you just need to `runghc Main`.

## Known Bugs (Features?) and Issues

- As part of the normalisation process, all queries and fragments are removed from URLs. Furthermore a '/' will be added to the end of any path segment without a file-extension (note that this includes *files without an extension*).
- When parsing HTML, the program will consider *everything*: this includes commented code and code that is within scripts.
- If the crawler finds a link (to a page on the current subdomain) that redirects, it will follow this link and crawl the redirected page, including if that page is actually on a different subdomain. Note however that it will not continue to crawl once it is on a different subdomain.

There may be other bugs present, as I have had to write code to parse URLs and HTML from scratch. In any other language, I would use libraries for these tasks, but unfortunately Haskell's libraries are rather underwhelming. :cry:

## Testing and Documentation

If you would like to reuse this code, make changes or simply investigate the code more deeply, you might find the following resources useful:
- A test suite can be run with the command `runghc Tests`.
- Haddock generated documentation for almost all included functions can be found [here](https://www.doc.ic.ac.uk/~js4416/public_html/100/simple-web-crawler/docs/) or in the Documentation folder.
