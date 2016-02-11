{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}

import Procedure
import RProcedure

data Device = Device { sync :: String -> IO String }

send :: Device -> Procedure a -> IO a
send d m = do
  r <- sync d (show m)
  return (readProcedureReply m r)

device :: Device
device = Device (execRProcedure . read)

main = do
  r <- send device Temperature
  print r
