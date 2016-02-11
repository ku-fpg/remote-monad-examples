{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures, RankNTypes, StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar

import DeepCommand
import DeepProcedure
import Expr
import Binder
import Transport
import DeviceDeep

import Data.Monoid

import Debug.Trace


------------------------------------------------------------------------

data Packet t a = Packet [Command] (t a)

deriving instance Show (t a) => Show (Packet t a)

instance Read (Transport t) => Read (Transport (Packet t)) where
  readsPrec d = readParen (d > 10) $ \ r0 ->
        [ (Transport $ Packet cs q,r3)
        | ("Packet",r1)     <- lex r0
        , (cs,r2)           <- reads r1
        , (Transport q,r3)  <- reads r2
        ]

instance Show (Transport (Packet Procedure)) where
  show (Transport t) = show t

evalPacket :: (t a -> DeviceMonad a) -> Packet t a -> DeviceMonad a
evalPacket eval (Packet cs m) = do
        mapM_ (evalCommandD . CommandD) cs
        eval m

------------------------------------------------------------------------

data Device = Device
  { sync  :: String -> IO String
  , async :: String -> IO ()
  }

newtype Remote a =
  Remote (ReaderT  Device
                  (StateT ([Command],Id) IO)
                  a
         )

deriving instance Monad Remote

instance Functor Remote where
  fmap = liftM

instance Applicative Remote where
  pure = return
  (<*>) = ap

printStringExpr :: StringExpr -> Remote ()
printStringExpr e = sendCommand (Say e)

sendCommand :: Command -> Remote ()
sendCommand cmd = Remote $ do
   (cs,u) <- get
   put (cs ++ [cmd],u)
   return ()

say :: StringExpr -> Remote ()
say txt = sendCommand (Say txt)

sendProcedure :: Procedure a -> Remote a
sendProcedure q = Remote $ do
   d <- ask
   (cs,u) <- get
   liftIO $ print $ show $ Packet cs q
   r <- liftIO $ sync d $ show $ Packet cs q
   liftIO $ print $ show $ Packet cs q
   put (mempty,u)
   return (readProcedureReply q r)

reply :: StringExpr -> Remote String
reply e = sendProcedure (Reply e)

bindBindee :: Bindee StringExpr -> Remote Id
bindBindee e = do
   i <- newId
   sendCommand (Bind i e)
   return i

temperature :: Remote StringExpr
temperature = do
  i <- bindBindee Temperature
  return (Var i)

newId :: Remote Id
newId = Remote $ do
        (cs,u) <- get
        put (cs,u+1)
        return u

send :: Device -> Remote a -> IO a
send d (Remote m) = do
  (r,(cs,u)) <- runStateT (runReaderT m d) (mempty,0)
  when (not (null cs)) $ do
    print $ show $ Packet (init cs) (CommandD (last cs))
    async d (show $ Packet (init cs) (CommandD (last cs)))
  return r

newDevice :: IO Device
newDevice = do
   envVar <- newMVar []

   let run :: (forall a . t a -> DeviceMonad a) -> Transport (Packet t) -> IO String
       run eval (Transport packet) = do
               print "Got here"
               env <- takeMVar envVar
               (r,env') <- runDeviceMonad (evalPacket eval packet) env
               putMVar envVar env'
               return (show r)

   return $ Device (       run evalProcedureD . read)
                   (void . run evalCommandD   . read)


main :: IO ()
main = do
 device <- newDevice
 r <- send device $ do
            t <- temperature
            say ("The temperature is " <> t <> "F")
            reply t
 print r
