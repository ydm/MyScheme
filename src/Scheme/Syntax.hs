{-# LANGUAGE OverloadedStrings #-}

module Scheme.Syntax where

import Scheme.ShowText
import qualified Data.Text as T


data Atom_ = Int_ Int
           | Nil
           | Symbol T.Text

data Sexp = Cons Sexp Sexp
          | Atom Atom_

-- instance Read -- TODO!

-- | Construct a sexp from lhs and rhs sexps.
cons :: Sexp -> Sexp -> Sexp
cons = Cons

-- | Wrap sexp in a singleton list.
consnil :: Sexp -> Sexp
consnil a = Cons a (Atom Nil)

-- | Create a sexp from list of sexps.
conslist :: [Sexp] -> Sexp
conslist = foldr Cons (Atom Nil)

len :: Sexp -> Int
len (Atom Nil) = 0
len (Atom _) = 1
len (Cons a b) = 1 + len b

-- Atom constructors

symbol :: T.Text -> Sexp
symbol = Atom . Symbol

int :: Int -> Sexp
int = Atom . Int_

nil :: Sexp
nil = Atom Nil


instance ShowText Atom_ where
  showt (Int_ x) = T.pack $ show x
  showt Nil = "()"
  showt (Symbol x) = x

instance ShowText Sexp where
  showt (Cons a b) = T.concat ["(", showt a, " . ", showt b, ")"]
  showt (Atom a) = showt a


-- +---------+
-- + AST ops +
-- +---------+

revsexp :: Sexp -> Sexp
revsexp = f nil
  where f :: Sexp -> Sexp -> Sexp
        f acc (Atom Nil) = acc
        -- f acc x@(Cons _ (Atom Nil)) = x
        f acc (Cons x y) = f (cons x acc) y

-- reverse x@(Cons _ (Atom Nil)) = x
-- reverse (Cons a b) = 


-- Examples

simple2 :: Sexp
simple2 = conslist $ map Atom [Symbol "+", Int_ 1, Int_ 2, Int_ 3]


-- OLD

-- type Signature = [Node] -> IO Node

-- data Node = Keyword T.Text
--           | Function T.Text Int Int Signature -- name, minArgs, maxArgs, fn
--           | List [Node]
--           | Nil
--           | NumInteger Int
--           | Quote Node
--           | Symbol T.Text
--           | Variable T.Text

-- instance Show Node where
--   show (Function name _ _ _) = T.unpack name
--   show (List []) = show Nil
--   show (List xs) = "(" ++ unwords (map show xs) ++ ")"
--   show Nil = "()"
--   show (NumInteger x) = show x
--   show (Quote x) = "(quote " ++ show x ++ ")"
--   show (Symbol x) = T.unpack x
--   -- show (Variable x) = T.unpack x


-- -- simple2 :: Sexp
-- -- simple2 = C (ASymbol "+")


-- simple :: Node
-- simple = List [Symbol "+", NumInteger 1, NumInteger 2, NumInteger 3]

-- wrongTypeArg :: Node
-- wrongTypeArg = List [Symbol "+", NumInteger 1, NumInteger 2, NumInteger 3, Symbol "qwe"]

-- wrong :: Node
-- wrong = List [Symbol "+", NumInteger 1, NumInteger 2, Symbol "qwe"]

-- please :: Node
-- -- please = List [Symbol "print", NumInteger 123, Symbol "qwe"]
-- please = List [Symbol "print", NumInteger 123, NumInteger 321]
