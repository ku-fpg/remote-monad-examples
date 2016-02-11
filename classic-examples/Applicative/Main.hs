{-# LANGUAGE RecursiveDo, GADTs, KindSignatures, StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity

import Command
import Procedure
import RPrim
import Data.Monoid

data Device = Device
  { sync  :: String -> IO String,
    async :: String -> IO ()
  }

newtype Remote a =
  Remote (WriterT [Prim] (State [String]) a)

deriving instance Functor Remote
deriving instance Applicative Remote
-- no monad instance

data Prim :: * where
  Command   ::           Command     -> Prim
  Procedure :: Show a => Procedure a -> Prim

deriving instance Show Prim

isCommand :: Prim -> Bool
isCommand (Command _)   = True
isCommand (Procedure _) = False

sendCommand :: Command -> Remote ()
sendCommand cmd = Remote (tell [Command cmd])

say :: String -> Remote ()
say txt = sendCommand (Say txt)

sendProcedure :: Show a => Procedure a -> Remote a
sendProcedure p = Remote $ do
   tell [Procedure p]
   ~(r:rs) <- get
   put rs
   return (readProcedureReply p r)

temperature :: Remote Int
temperature = sendProcedure Temperature

toast :: Int -> Remote ()
toast n = sendProcedure (Toast n)

send :: Device -> Remote a -> IO a
send d (Remote m) = do
  rec let ((a,ps),_) = runState (runWriterT m) r
      r <- if all isCommand ps
             then do async d (show ps)
                     return []
             else do str <- sync d (show ps)
                     return (read str)
  return a


device :: Device
device = Device (liftM show . execRPrims . read)
                (void       . execRPrims . read)

main :: IO ()
main = do
    (t1,t2) <- send device $ liftA2 (,)
            (say "Good Morning" *> temperature)
            (toast 5 *> say "Toast!" *> temperature)
    send device $ say $
            show t1 ++ " before and " ++
            show t2 ++ " after"
