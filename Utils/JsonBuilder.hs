{-# LANGUAGE OverloadedStrings #-}

module Utils.JsonBuilder where

import Utils.HtmlParser

--------------------------------------------------------------------------------
-- SHOW JSON FUNCTIONS
-- Functions to pretty print a JSON-format String representation for [Website]

showJson :: [Webpage] -> String
showJson ws
  = "[\n" ++ concatMap showJson' ws ++ "]\n"

showJson' :: Webpage -> String
showJson' w
  = "\t{\n"
    ++ "\t\t \"url\": " ++ show (url w) ++ ",\n"
    ++ "\t\t \"assets\": " ++ "[\n"
      ++ concatMap (("\t\t\t" ++) . (++ ",\n") . show) (assets w)
    ++ "\t\t]\n"
  ++ "\t},\n"
