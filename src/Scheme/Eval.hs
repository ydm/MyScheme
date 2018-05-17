{-# LANGUAGE OverloadedStrings #-}

module Scheme.Eval where

import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Scheme.Syntax


data Env = Env
  (Maybe Env) -- parent env
  [Node]        -- lookup table
  deriving Show


-- | Get the first matching element.
first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first f (x : xs)
  | f x = Just x
  | otherwise = first f xs

name :: Node -> T.Text
name (Function x _ _ _) = x
name (Symbol x) = x
name _ = ""

fromEnv :: T.Text -> Env -> Maybe Node
fromEnv x (Env parent table)
  | isNothing node = maybe Nothing (fromEnv x) parent
  | otherwise = node
  where node = first ((x==) . name) table

-- | Check the number of function arguments before returning
check :: T.Text -> Int -> Int -> Int -> IO a -> IO a
check name min max n x
  | min == max && min /= n = fail $ expected ++ min' ++ args
  | n < min || max < n    = fail $ (expected ++ "between " ++ min'
                                    ++ " and " ++ max' ++ args)
  | otherwise = x
  where expected = "function " ++ T.unpack name ++ " expected "
        min' = show min
        max' = show max
        args = " arg(s), got " ++ show n

call :: Node -> Signature
call (Function name min max fn) = \xs -> check name min max (length xs) (fn xs)
call x = fail $ "invalid function" ++ show x

eval :: Node -> StateT Env IO Node
eval x@(NumInteger _) = StateT $ \env -> return (x, env)
eval (Quote x) = StateT $ \env -> return (x, env)
eval (Symbol x) = StateT $ \env -> maybe
  (fail $ "void variable " ++ T.unpack x)
  (\node -> return (node, env))
  (fromEnv x env)
eval (List (Symbol x : xs)) = StateT $ \env -> maybe
  (fail $ "void function " ++ T.unpack x)
  (\node -> flip (,) env <$> call node xs)
  (fromEnv x env)

core = Env Nothing
  [ Function "+" 1 maxBound (return . sum)
  , Function "print" 1 1 $ \[x] -> putStr "\n" >> putStrLn (show x) >> return Nil
  , Symbol "+"
  ]
