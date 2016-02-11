{-# LANGUAGE GADTs, KindSignatures #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State


import Command
import Procedure
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

send:: (Show a) =>Device -> Remote a -> IO a
send d m = do (a,st) <-runStateT (sendRemote m) ([]++)
              let cmds = st []
              when (not (null cmds)) $ async device $ show (Packet cmds Nothing)
              return a

   where
     sendRemote :: Remote a ->StateT ([Command]->[Command]) IO a
     sendRemote (Return a)  = return a  
     sendRemote (Bind f k)  = sendRemote f >>= (sendRemote . k)
     sendRemote (Command c) = modify $ \s -> s . ([c]++)
     sendRemote (Procedure p) = do s <- get
                                   put ([]++)
                                   res <- liftIO $ sync d $ show (Packet (s []) (Just p) )
                                   return $ read res
device :: Device
device = Device (actor . read) 
                (void . actor . read)
  where
    -- ExecRPacket handles show
    actor :: RPacket -> IO String
    actor (cmd) = do {r <- execRPacket cmd ; return r }

example = do
            Command (Say "1")
            Command (Say "9")
            Procedure (Toast 3)
            n <- Procedure (Temperature)
            Command (Say ("The temperature was: "++ (show (n))))
            return n



main =  send device example
