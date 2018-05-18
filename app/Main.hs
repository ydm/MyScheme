module Main where

import Control.Monad.Trans.State (runStateT)
import qualified Data.Text.IO as TIO
import Scheme.Parser
import Scheme.Eval
import Scheme.ShowText


loop :: Env -> IO ()
loop env = do
  line <- getLine
  let x = parse line
  (x, _) <- runStateT (eval x) env
  TIO.putStrLn $ showt x
  -- return 
  -- let x = parse line in runState (eval x) env >>= loop


main :: IO ()
main = loop initial >> main
-- main = getLine >>= print
