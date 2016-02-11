{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Class

import Command
import RCommand
import Procedure
import RProcedure

data Device = Device
  { sync  :: String -> IO String
  , async :: String -> IO ()
  }

newtype Remote a = Remote (ReaderT Device IO a)

deriving instance Monad Remote

instance Functor Remote where
  fmap = liftM

instance Applicative Remote where
  pure = return
  (<*>) = ap

sendCommand :: Command -> Remote ()
sendCommand m = Remote $ do
  d <- ask
  liftIO (async d (show m))
  return ()

say :: String -> Remote ()
say txt = sendCommand (Say txt)

sendProcedure :: Procedure a -> Remote a
sendProcedure m = Remote $ do
  d <- ask
  r <- liftIO (sync d (show m))
  return (readProcedureReply m r)

temperature :: Remote Int
temperature = sendProcedure Temperature

toast :: Int -> Remote ()
toast n = sendProcedure (Toast n)

send :: Device -> Remote a -> IO a
send d (Remote m) = runReaderT m d

device :: Device
device = Device (execRProcedure . read)
                (execRCommand   . read)

main :: IO ()
main = do
 t <- send device $ do
            say "Howdy doodly do"
            say "How about a muffin?"
            t <- temperature
            say (show t ++ "F")
            return t
 when (t < 50) $ do
   send device $ say "A muffin will warm you up"

newtype Natural m n = Natural (forall a . m a -> n a)

-- I think this is something much more general than just Remoteness.
newtype RemoteT (m :: * -> *) a where
  RemoteT :: ReaderT (Natural m IO) IO a -> RemoteT m a

deriving instance Monad m => Monad (RemoteT m)
deriving instance Monad m => Applicative (RemoteT m)
deriving instance Monad m => Functor (RemoteT m)

instance MonadTrans RemoteT where
        lift m = RemoteT $ do
                Natural send <- ask
                liftIO $ send m

runRemoteT :: (forall a . m a -> IO a) -> RemoteT m a -> IO a
runRemoteT f (RemoteT m) = runReaderT m (Natural f)
