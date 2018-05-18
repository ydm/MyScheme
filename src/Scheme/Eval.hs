{-# LANGUAGE OverloadedStrings #-}

module Scheme.Eval where

import Control.Monad.Trans.State (StateT(..))
import Data.HashMap.Strict as M (HashMap(..), fromList, lookup)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Scheme.Syntax


-- +-------------+
-- + Environment |
-- +-------------+

data Env = Env
  (Maybe Env)           -- parent env
  (HashMap T.Text Node) -- lookup table
  deriving Show

lookup' :: T.Text -> Env -> Maybe Node
lookup' x (Env parent table) = M.lookup x table

core = Env Nothing $ fromList
  [ ("+" , Function "+" 1 maxBound _sum)
  , ("print" , Function "print" 1 1 _print)
  , ("something", Symbol "+")
  ]

_sum :: [Node] -> IO Node
_sum = accum 0
  where accum :: Int -> [Node] -> IO Node
        accum s [] = return $ NumInteger s
        accum s (NumInteger x : xs) = accum (s + x) xs
        accum _ [x] = failWrongTypeArg $ show x

_print :: [Node] -> IO Node
_print [x] = putStr "\n" >> putStrLn (show x) >> return Nil


-- +------------+
-- + Evaluation |
-- +------------+

-- | Check the number of function arguments before returning
check :: T.Text -> Int -> Int -> Int -> IO a -> IO a
check name min max len x
  | min == max && min /= len = failWrongNumArgs1 name min len
  | len  < min || max < len = failWrongNumArgs2 name min max len
  | otherwise = x

call :: Node -> Signature
call (Function name min max fn) = \xs -> check name min max (length xs) (fn xs)
call x = const $ failInvalidFunction $ show x

eval :: Node -> StateT Env IO Node
eval x@(NumInteger _) = StateT $ \env -> return (x, env)
eval (Quote x) = StateT $ \env -> return (x, env)
eval (Symbol x) = StateT $ \env -> maybe
  (failVoidVariable x)
  (\node -> return (node, env))
  (lookup' x env)
eval (List (Symbol x : xs)) = StateT $ \env -> maybe
  (failVoidFunction x)
  (\node -> flip (,) env <$> call node xs)
  (lookup' x env)


-- +----------------+
-- + Error messages |
-- +----------------+

failInvalidFunction :: String -> IO a
failInvalidFunction x = fail $ "invalid function: " ++ x

failVoidVariable :: T.Text -> IO a
failVoidVariable x = fail $ "void variable: " ++ T.unpack x

failVoidFunction :: T.Text -> IO a
failVoidFunction x = fail $ "void function: " ++ T.unpack x

failWrongNumArgs1 :: T.Text -> Int -> Int -> IO a
failWrongNumArgs1 fn a n = fail $ concat
  [ "function ", T.unpack fn
  , " expected ", show a
  , " arg(s), got ", show n ]

failWrongNumArgs2 :: T.Text -> Int -> Int -> Int -> IO a
failWrongNumArgs2 fn a b n = fail $ concat
  [ "function ", T.unpack fn
  , " expected between ", show a, " and ", show b
  , " arg(s), got ", show n ]

failWrongTypeArg :: String -> IO a
failWrongTypeArg x = fail $ "wrong type argument: " ++ x
