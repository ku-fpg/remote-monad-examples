module RPrim
  (RPrim,execRPrims)
where

import RCommand
import RProcedure

data RPrim = Command RCommand
           | Procedure RProcedure
             deriving Read

execRPrims :: [RPrim] -> IO [String]
execRPrims [] = return []
execRPrims (Command c : ps) = do
         execRCommand c
         execRPrims ps
execRPrims (Procedure p : ps) = do
         r  <- execRProcedure p
         rs <- execRPrims ps
         return (r:rs)
