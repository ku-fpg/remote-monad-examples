{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}



import  Control.Remote.Monad 
import  Control.Remote.Monad.Packet.Weak
import  Control.Natural (nat, run)
import  System.Random (randomRIO)

import Types

weakDispatch :: WeakPacket MyCommand MyProcedure a -> IO a
weakDispatch (Procedure x) = procedureDispatch x
weakDispatch (Command x )  = commandDispatch x


procedureDispatch :: MyProcedure a -> IO a
-- Our screen is a fixed size of 100 x 100
procedureDispatch (Screen) = return (100,100)
procedureDispatch (Uptime) = randomRIO (10,1000)


commandDispatch :: MyCommand -> IO ()
commandDispatch (Color c) = undefined
commandDispatch (Stroke style)= undefined
commandDispatch (Draw sh)= do putStrLn "Start Remote Draw\n"
                              draw sh
                              putStrLn "\nEnd Remote Draw"
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


send :: RemoteMonad MyCommand MyProcedure a -> IO a
send = run $ runMonad $ nat weakDispatch


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
       
      putStrLn $  "Local: Screen size: "++ (show screenSize)
      putStrLn $  "Local: Uptime "++ (show time)
