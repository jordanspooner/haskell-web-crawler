# Haskell Web Crawler

## Functionality

This is a concurrent web crawler that ouptus to STDOUT a JSON formatted list of all the reachable pages on that subdomain, and for each page gives its URL and a list of any static assets which it links to.

### Example

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

### Requirements

This program requires GHC and several dependencies, which can be installed using Cabal. I suggest downloading and installing the Haskell Platform (if you don't already have it), which is available on most package installers. E.g. using `apt-get`, you can install the Haskell Platform with the command `sudo apt-get install haskell-platform`.

To install the dependencies, `cd` into the `simple-web-crawler` directory and run the command `cabal install --only-dependencies`.

### Compilation

To compile the program so that you can run it in a multithreaded environment, you should use the command `ghc -O2 --make Main.hs -threaded -rtsopts`.

* `ghc` is the Glasgow Haskell compiler. It compiles the Haskell code and links it to a runtime system.
* `-O2` specifies the level of optimisation to be the highest. Since there isn't too much code to compile, compilation should still be fairly quick.
* `--make Main.hs` will build the Haskell program `Main.hs`, automatically figuring out any dependencies.
* `-threaded` uses the threaded runtime (allowing the program to run with multiple threads).
* `-rtsopts` allows us to change some of the options for the runtime system.

### Troubleshooting

If you are getting compiler errors, try the following fix:

* First recreate the cabal package. You can use `cabal init` to do this. Just choosing the defaults on every stage should be fine.
* Then reinstall the dependencies using `cabal install --only-dependencies --reinstall`.

If you're still getting errors, raise an issue and I'll have a look into it!

## Usage

The program can be run with the command `./Main +RTS -Nx`, where you should change `x` (the number of threads) to the number of cores you have (or a slightly higher value). If you're not sure, just omit the `x`, and the runtime will choose a value of `x` itself.

## Known Bugs (Features?) and Issues

- As part of the normalisation process, all queries and fragments are removed from URLs.
- When parsing HTML, the program will consider *everything*: this includes commented code and code that is within scripts.
- If the crawler finds a link (to a page on the current subdomain) that redirects, it will follow this link and crawl the redirected page, including if that page is actually on a different subdomain. Note however that it will not continue to crawl once it is on a different subdomain.

There may be other bugs present, as I have had to write code to parse URLs and HTML from scratch. In any other language, I would use libraries for these rather complex tasks, but unfortunately Haskell's libraries are rather underwhelming. :cry:

## Testing and Documentation

If you would like to reuse this code, make changes or simply investigate the code more deeply, you might find the following resources useful:
- A test suite can be run with the command `runghc Tests`.
- Haddock generated documentation for almost all included functions can be found [here](https://www.doc.ic.ac.uk/~js4416/100/simple-web-crawler/docs/) or in the `Documentation` directory.
