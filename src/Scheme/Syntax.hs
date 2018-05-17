{-# LANGUAGE OverloadedStrings #-}

module Scheme.Syntax where

import qualified Data.Text as T

type Signature = [Node] -> IO Node

data Node = Keyword T.Text
          | Function
            { fnName :: T.Text
            , fnMinArgs :: Int
            , fnMaxArgs :: Int
            , fn :: Signature
            }
          | List [Node]
          | Nil
          | NumInteger Int
          | Quote Node
          | Symbol T.Text
          | Variable T.Text

instance Show Node where
  show (Function name _ _ _) = T.unpack name
  show (List []) = show Nil
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (NumInteger x) = show x
  show (Quote x) = "(quote " ++ show x ++ ")"
  show (Symbol x) = T.unpack x
  -- show (Variable x) = T.unpack x
  show Nil = "()"

instance Num Node where
  (NumInteger x) + (NumInteger y) = NumInteger $ x + y
  (NumInteger x) * (NumInteger y) = NumInteger $ x * y
  abs (NumInteger x)              = NumInteger $ abs x
  signum (NumInteger x)           = NumInteger $ signum x
  fromInteger x                   = NumInteger $ fromInteger x
  negate (NumInteger x)           = NumInteger $ negate x


simple :: Node
simple = List [Symbol "+", NumInteger 1, NumInteger 2, NumInteger 3]

wrong :: Node
wrong = List [Symbol "+", NumInteger 1, NumInteger 2, Symbol "qwe"]

please :: Node
-- please = List [Symbol "print", NumInteger 123, Symbol "qwe"]
please = List [Symbol "print", NumInteger 123, NumInteger 321]
