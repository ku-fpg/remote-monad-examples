{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


import  Control.Monad (void)
import  Control.Remote.Monad 
import  Control.Remote.Monad.Packet.Weak
import  Control.Natural (nat, run)
import  Types
import  Data.Aeson
import Control.Lens((^.))
import Network.Wreq


------------------------ Server Code ------------------------
weakDispatch :: String -> WeakPacket MyCommand MyProcedure a -> IO a
weakDispatch url (Command x) = void $  post url (toJSON x)
weakDispatch url (Procedure x) = do r <- post url (toJSON x)
                                    return $ r ^.responseBody 


send :: RemoteMonad MyCommand MyProcedure a -> IO a
send = run $ runMonad $ nat (weakDispatch "http://localhost:3000/test")


main:: IO()
main =do 
      putStrLn "Let's draw things"
      (screenSize, time) <- send $ do
                       drawShape diamond
                       x<- getScreen
                       drawShape rectangle
                       drawShape square
                       y <- getUptime
                       return (x,y)
       
      putStrLn $  "Local: Remote Screen size: "++ (show screenSize)
      putStrLn $  "Local: Remote Uptime "++ (show time)
