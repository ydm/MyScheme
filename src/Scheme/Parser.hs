module Scheme.Parser where

import Debug.Trace
import Scheme.ShowText

import Data.Char
import qualified Data.Text as T
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

parse :: String -> Sexp
parse = readFromTokens (Builder []) . tokenize


-- +---------+
-- + Builder |
-- +---------+

data Builder = Builder [Sexp]

-- instance Show Builder where
--   show (Builder xs) = unwords $ map (T.unpack . showt) xs

-- | Also reverses the previous list.
begin :: Builder -> Builder
begin (Builder []) = Builder [nil]
begin (Builder xs) = Builder $ nil:xs

append :: Builder -> Sexp -> Builder
append (Builder (x:xs)) y = Builder $ (Cons y x) : xs

done :: Builder -> Builder
done (Builder xs) = Builder $ reverse $ map revsexp xs

progn :: Builder -> Sexp
progn (Builder xs) = head xs  -- TODO! :)

readFromTokens :: Builder -> [String] -> Sexp
readFromTokens b [] = progn $ done b
readFromTokens b ("(":xs) = readFromTokens (begin b) xs
readFromTokens b (")":xs) = readFromTokens b xs
readFromTokens b (x@(y:_):xs)
  | isDigit y = readFromTokens (append b $ int (read x :: Int)) xs
  | otherwise = readFromTokens (append b $ symbol $ T.pack x) xs


