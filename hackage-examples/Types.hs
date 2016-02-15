{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Types where

import Control.Remote.Monad

data Shape :: * where
   Square    :: Int ->        Shape
   Diamond   ::               Shape
   Rectangle :: Int -> Int -> Shape


square :: Shape
square = Square 4 

diamond :: Shape
diamond = Diamond

rectangle :: Shape
rectangle = Rectangle 5 3

data MyCommand :: * where
   Color   :: String  -> MyCommand
   Stroke  :: String  -> MyCommand
   Draw    :: Shape   -> MyCommand

data MyProcedure :: * -> * where
   Screen ::  MyProcedure (Int,Int)
   Uptime ::  MyProcedure Double


getScreen :: RemoteMonad MyCommand MyProcedure (Int,Int)
getScreen = procedure Screen

drawShape :: Shape -> RemoteMonad MyCommand MyProcedure ()
drawShape sh = command (Draw sh) 

getUptime :: RemoteMonad MyCommand MyProcedure Double
getUptime = procedure Uptime
