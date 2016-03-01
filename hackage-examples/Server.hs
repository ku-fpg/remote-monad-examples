{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings  #-}


import  Control.Monad.IO.Class
import  Control.Remote.Monad 
import qualified  Control.Remote.Monad.Packet.Applicative as AP
import  Control.Remote.Monad.Binary
import  Control.Remote.Monad.Transport
import  Control.Natural (nat, run)
import  System.Random (randomRIO)
import  Network.Transport
import  Network.Transport.TCP

import Types

------------------------ Server Code ------------------------

{-
data Call  = Call Method Args
 deriving (Show)
type Method = String
type Args = Maybe Value


instance FromJSON Call where
   parseJSON (Object v) = Call <$> v .: "method"
                               <*> v .:? "params" 

serve port path = scotty port $ post (literal path) $ scottyReceive 


scottyReceive :: ActionM ()
scottyReceive = do x <- jsonData
                   case fromJSON x of
                    Success (Call meth args) -> case meth of
                                          "screen" -> do r <- liftIO $ procedureDispatch Screen 
                                                         S.json r
                                          "uptime" -> do r <- liftIO $ procedureDispatch Uptime 
                                                         S.json r
                                          "draw" -> do case args of
                                                        Just a -> case fromJSON a of
                                                                     Success sh -> do liftIO $ commandDispatch $ Draw sh
                                                                     Error s -> liftIO $ do print s
                                                                                            print a
                                                        Nothing -> S.text "An error has occured"
                                          _ -> S.text "Method not found"

-}





dispatch :: AP.ApplicativePacket MyCommand MyProcedure a -> IO a
dispatch (AP.Command c) = commandDispatch c
dispatch (AP.Procedure p) = procedureDispatch p
dispatch (AP.Zip f a b) = f <$> dispatch a <*> dispatch b
dispatch (AP.Pure a ) = return a

procedureDispatch :: MyProcedure a ->  IO a
-- Our screen is a fixed size of 100 x 100
procedureDispatch (Screen) =  return (100,100)
procedureDispatch (Uptime) =  randomRIO (10,1000)


commandDispatch :: (MonadIO m) => MyCommand -> m ()
commandDispatch (Color c) = return ()
commandDispatch (Draw sh)= do liftIO $ putStrLn "Start Remote Draw\n"
                              liftIO $ draw sh
                              liftIO $ putStrLn "\nEnd Remote Draw"
                  where draw:: Shape -> IO()
                        draw (Square w )     = mkLines w w
                        draw (Rectangle w h) = mkLines w h
                        draw (Diamond)       = do 
                                       
                                       putStrLn "     *     "
                                       putStrLn "   * * *   "
                                       putStrLn " * * * * * "
                                       putStrLn "   * * *   "
                                       putStrLn "     *     "
                        line :: Int -> IO ()
                        line x 
                          |  x <= 0     = return ()
                          |  otherwise  = do putStr " * " 
                                             line (x -1)
                        
                        mkLines :: Int -> Int -> IO ()
                        mkLines w h 
                         | h <= 0    = return ()
                         | otherwise = do line w
                                          putStrLn ""
                                          mkLines w (h-1)




main:: IO()
main = do 
    Right transport <- createTransport "localhost" "30179" defaultTCPParameters
    transportServer transport $ server $ nat dispatch
    return ()

