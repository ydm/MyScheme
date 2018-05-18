{-# LANGUAGE OverloadedStrings #-}

module Scheme.Syntax where

import qualified Data.Text as T

type Signature = [Node] -> IO Node

data Node = Keyword T.Text
          | Function T.Text Int Int Signature -- name, minArgs, maxArgs, fn
          | List [Node]
          | Nil
          | NumInteger Int
          | Quote Node
          | Symbol T.Text
          | Variable T.Text

data Sexp = Atom a
          | Cons Sexp Sexp

instance Show Node where
  show (Function name _ _ _) = T.unpack name
  show (List []) = show Nil
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show Nil = "()"
  show (NumInteger x) = show x
  show (Quote x) = "(quote " ++ show x ++ ")"
  show (Symbol x) = T.unpack x
  -- show (Variable x) = T.unpack x


simple :: Node
simple = List [Symbol "+", NumInteger 1, NumInteger 2, NumInteger 3]

wrongTypeArg :: Node
wrongTypeArg = List [Symbol "+", NumInteger 1, NumInteger 2, NumInteger 3, Symbol "qwe"]

wrong :: Node
wrong = List [Symbol "+", NumInteger 1, NumInteger 2, Symbol "qwe"]

please :: Node
-- please = List [Symbol "print", NumInteger 123, Symbol "qwe"]
please = List [Symbol "print", NumInteger 123, NumInteger 321]
