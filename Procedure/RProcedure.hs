module RProcedure
  (RProcedure, execRProcedure)
where

import System.Random
import Control.Concurrent

data RProcedure = Temperature 
                | Toast Int
                  deriving Read

execRProcedure :: RProcedure -> IO String
execRProcedure Temperature  = do
  t <- randomRIO (50, 100 :: Int)
  return (show t)
execRProcedure (Toast n)    = do
  putStrLn ("Remote: Toasting...")
  threadDelay (1000 * 1000 * n)
  putStrLn ("Remote: Done!")
  return (show ())
