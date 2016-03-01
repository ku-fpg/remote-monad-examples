{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


import  Control.Monad (void)
import  Control.Remote.Monad.Binary
import  Control.Remote.Monad.Transport
import  Control.Natural 
import  Network.Transport hiding (send)
import  Network.Transport.TCP

import  Types



main:: IO()
main =do 
      Right transport <- createTransport "localhost" "30178" defaultTCPParameters
      Nat f <- transportClient transport $ encodeEndPointAddress "localhost" "30179" 0
      let session = monadClient f

      putStrLn "Let's draw things"
      (screenSize, time) <- send session$ do
                       drawShape diamond
                       x<- getScreen
                       drawShape rectangle
                       drawShape square
                       y <- getUptime
                       return (x,y)
       
      putStrLn $  "Local: Remote Screen size: "++ (show screenSize)
      putStrLn $  "Local: Remote Uptime "++ (show time)
