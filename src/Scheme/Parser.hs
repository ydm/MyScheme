module Scheme.Parser where

import Data.Char
import Scheme.Syntax


isDelim :: Char -> Bool
isDelim c = c `elem` "'()"

tokenize :: String -> [String]
tokenize [] = []
tokenize a@(x:xs)
  | isSpace x = tokenize xs
  | isDelim x = [x] : tokenize xs
  | otherwise = let (token, rest) = eat "" a in token : tokenize rest
  where eat :: String -> String -> (String, String)
        eat acc b@(y:ys)
          | isSpace y || isDelim y = (reverse acc, b)
          | otherwise = eat (y:acc) ys

-- readFromTokens :: [String] -> Node
-- readFromTokens ('\'':xs) = Quote $ readFromTokens xs
-- readFromTokens ('(':xs) = List $ readFromTokens
