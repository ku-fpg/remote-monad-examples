{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, StandaloneDeriving, FlexibleInstances #-}

import Control.Applicative
import Control.Monad

import Command
import RCommand
import Transport
import Procedure
import RProcedure
import Packet
import RPacket

data Device = Device 
  { sync  :: String -> IO String
  , async :: String -> IO ()
  }

data Remote ::  * -> * where
  Command :: Command -> Remote ()
  Procedure   :: Read a => Procedure a -> Remote a
  Bind    :: Remote a -> (a -> Remote b) -> Remote b
  Return  :: a -> Remote a

instance Monad Remote where
  return = Return
  (>>=) = Bind
  
instance Functor Remote where
  fmap = liftM
  
instance Applicative Remote where
  pure = return
  (<*>) = ap

-- data Packet :: * -> * where
--   Command_ :: Command () -> Packet a -> Packet a
--   Procedure_   :: Procedure a ->                Packet a
--   Return_  ::                           Packet ()

-- instance Show (Packet a) where
--  show (Command_ c p)    = show c ++ ";" ++ show p
--  show (Procedure_ q)        = show q
--  show (Return_)         = "return"

-- instance Read (Transport Packet) where
--   readsPrec _ r0 = 
--         [ (Transport $ Command_ c p,r2)
--         | (c,r1)      <- reads r0
--         , (";",r2)    <- lex r1
--         , (Transport p,r2) <- reads r2
--         ] ++
--         [ (Transport $ Procedure_ q, r1)
--         | (Transport q,r1) <- reads r0
--         ] ++
--         [ (Transport $ Return_, r1)
--         | ("return",r1) <- lex r0
--         ]
        

-- send :: Device -> Remote a -> IO a
-- send ch m = sendRemote m id
--   where 
--      sendRemote :: Remote a -> (forall b. Packet b -> Packet b) -> IO a
--      sendRemote (Return a) pk = sendReturn a pk
--      sendRemote (Bind m k) pk = sendBind m k pk
--      sendRemote r pk          = sendBind r Return pk

--      sendReturn :: a -> (forall b. Packet b -> Packet b) -> IO a
--      sendReturn a pk = do 
--          async ch $ show (pk Return_)
--          return a

--      sendBind :: Remote a 
--               -> (a -> Remote b) 
--               -> (forall b. Packet b -> Packet b)
--               -> IO b
--      sendBind (Return a) k pk = 
--              sendRemote (k a) pk
--      sendBind (Bind m h) k pk = 
--              sendBind m (\ r -> Bind (h r) k) pk
--      sendBind (Command m) k pk = do
--              sendRemote (k ()) (pk . Command_ m)
--      sendBind (Procedure q) k pk = do
--              rep <- sync ch (show (pk $ Procedure_ q))
--              let r = readProcedureReply q rep
--              sendRemote (k r) id

send:: Device -> Remote a -> IO a
send d m = sendRemote m 
   where
     sendRemote :: Remote a -> IO a
     sendRemote (Return a)  = return a
     sendRemote (Bind f k)  = undefined
     sendRemote (Command c) = async d $ show c
     sendRemote (Procedure p) = do res <- sync d $ show p
                                   return $ read res
device :: Device
device = Device (actor . read) 
                (void . actor . read)
  where
    actor :: RPacket -> IO String
    actor (cmd) = do { r <- execRPacket cmd ; return (show r) }

example1 = do
            Command (Say "1")
            Command (Say "9")
            n <- Procedure (Temperature)
            Command (Say (show n))

example2 = Command(Say "1")

main = send device example2


