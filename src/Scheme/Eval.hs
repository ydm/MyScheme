{-# LANGUAGE OverloadedStrings #-}

module Scheme.Eval where

import Control.Monad.Trans.State (StateT(..))
import Data.HashMap.Strict as M (HashMap(..), empty, fromList, lookup, toList)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Scheme.Syntax
import Scheme.ShowText


-- +----------------+
-- + Error messages |
-- +----------------+

failt :: T.Text -> IO a
failt = fail . T.unpack

failInvalidFunction :: (ShowText a) => a -> IO b
failInvalidFunction x = failt $ T.concat ["invalid function: ", showt x]

-- TODO
failVoidVariable :: T.Text -> IO a
failVoidVariable x = fail $ "void variable: " ++ T.unpack x

failVoidFunction :: T.Text -> IO a
failVoidFunction x = failt $ T.concat ["void function: ", x]

-- TODO
failWrongNumArgs1 :: T.Text -> Int -> Int -> IO a
failWrongNumArgs1 fn a n = fail $ concat
  [ "function ", T.unpack fn
  , " expected ", show a
  , " arg(s), got ", show n ]

-- TODO
failWrongNumArgs2 :: T.Text -> Int -> Int -> Int -> IO a
failWrongNumArgs2 fn a b n = fail $ concat
  [ "function ", T.unpack fn
  , " expected between ", show a, " and ", show b
  , " arg(s), got ", show n ]

failWrongTypeArg :: (ShowText a) => a -> IO b
failWrongTypeArg x = failt $ T.concat ["wrong type argument: ", showt x]


-- +----------+
-- + Builtins |
-- +----------+

-- | name minArgs maxArgs fn
data Func = Func T.Text Int Int (Sexp -> IO Sexp)

instance ShowText Func where
  showt (Func name _ _ _) = name

-- | Combine two int atoms.
combine :: (Int -> Int -> Int) -> Sexp -> Sexp -> IO Sexp
combine f (Atom (Int_ x)) (Atom (Int_ y)) = return $ Atom $ Int_ $ f x y
combine _               x (Atom (Int_ _)) = failWrongTypeArg x
combine _               _               y = failWrongTypeArg y

_foldLeft :: (Sexp -> Sexp -> IO Sexp) -> Sexp -> Sexp -> IO Sexp
_foldLeft fn initial (Atom Nil) = return initial
_foldLeft fn initial (Cons a b) = fn initial a >>= \x -> _foldLeft fn x b

_reduceLeft :: (Sexp -> Sexp -> IO Sexp) -> Sexp -> Sexp -> IO Sexp
_reduceLeft fn initial (Atom Nil) = return initial
_reduceLeft fn _ (Cons x (Atom Nil)) = return x
_reduceLeft fn _ (Cons x y) = _foldLeft fn x y

_minus :: Sexp -> IO Sexp
_minus = _reduceLeft (combine (-)) $ int 0

_plus :: Sexp -> IO Sexp
_plus = _reduceLeft (combine (+)) $ int 0

_mult :: Sexp -> IO Sexp
_mult = _reduceLeft (combine (*)) $ int 1

-- TODO: Implmenet the Lisp / for Int
-- _div :: Sexp -> IO Sexp
-- _div = _reduceLeft (combine (/)) $ int 1

_print :: Sexp -> IO Sexp
_print (Atom x) = TIO.putStr "\n" >> TIO.putStrLn (showt x) >> return nil

_quote :: Sexp -> IO Sexp
_quote = return . id

builtins :: HashMap T.Text Func
builtins = fromList [ ("*", Func "*" 1 maxBound _mult)
                    , ("+", Func "+" 1 maxBound _plus)
                    , ("-", Func "-" 1 maxBound _minus)
                    -- , ("/", Func "/" 1 maxBound _div)
                    , ("print", Func "print" 1 1 _print)
                    , ("quote", Func "quote" 1 1 _quote)
                    ]

check :: T.Text -> Int -> Int -> Int -> IO a -> IO a
check name min max len x
  | min == max && min /= len = failWrongNumArgs1 name min len
  | len  < min || max < len = failWrongNumArgs2 name min max len
  | otherwise = x

call' :: Func -> Sexp -> IO Sexp
call' (Func name min max fn) sexp = check name min max (len sexp) (fn sexp)

call :: Sexp -> IO Sexp
call (Cons (Atom (Symbol x)) args) = maybe
  (failVoidFunction x)
  (flip call' args)
  (M.lookup x builtins)
call (Cons (Atom x) _) = failInvalidFunction x
call (Atom x) = failInvalidFunction x


-- +-------------+
-- + Environment |
-- +-------------+

data Env = Env
  (HashMap T.Text Sexp) -- lookup table
  (Maybe Env)           -- parent env

instance (ShowText a, ShowText b) => ShowText (a, b) where
  showt (a, b) = T.concat ["(", showt a, ", ", showt b, ")"]

instance (ShowText a, ShowText b) => ShowText (HashMap a b) where
  showt m = T.concat ["[", T.intercalate ", " $ map showt $ M.toList m, "]"]

instance ShowText Env where
  showt (Env m (Just x)) = T.concat [showt (Env m Nothing), " parent=", showt x]
  showt (Env m Nothing) = T.concat ["Env table=", showt m]

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes

lookup' :: T.Text -> Env -> Maybe Sexp
lookup' x (Env table parent) = firstJust
  [ M.lookup x table
  , parent >>= lookup' x
  ]

initial :: Env
initial = Env M.empty Nothing

-- +------------+
-- + Evaluation |
-- +------------+

eval :: Sexp -> StateT Env IO Sexp
eval xs@(Cons (Atom (Symbol _)) _) = StateT $ \env -> do
  ans <- call xs
  return (ans, env)
eval (Atom Nil) = StateT $ \env -> return (nil, env)
eval (Atom (Symbol x)) = StateT $ \env ->
  return (maybe nil id (lookup' x env), env)
eval x@(Atom (Int_ _)) = StateT $ \env -> return (x, env)
