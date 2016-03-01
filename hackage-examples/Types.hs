{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Types where

import Control.Remote.Monad
import Control.Remote.Monad.Binary
import Data.Binary

data Shape :: * where
   Square    :: Int ->        Shape
   Diamond   ::               Shape
   Rectangle :: Int -> Int -> Shape
  deriving (Show)

instance Binary Shape where
   put (Square n)      = do put (0 ::Word8)
                            put n
   put (Diamond)       =  put (1 :: Word8)
   put (Rectangle w h) = do put (2 :: Word8)
                            put w
                            put h
   get = do i <- get
            case i :: Word8 of
              0 -> do n <- get 
                      return $ Square n
              1 -> return Diamond
              2 -> do w <- get
                      h <- get
                      return $ Rectangle w h


square :: Shape
square = Square 4 

diamond :: Shape
diamond = Diamond

rectangle :: Shape
rectangle = Rectangle 5 3

data MyCommand :: * where
   Color   :: String  -> MyCommand
   Draw    :: Shape   -> MyCommand

instance Binary MyCommand where
    put (Color s) = do put (0 :: Word8)
                       put s
    put (Draw sh) = do put (1 :: Word8)
                       put sh

    get = do i <- get
             case i :: Word8 of
               0 ->do s <- get
                      return $ Color s
               1 ->do sh <- get
                      return $ Draw sh


data MyProcedure :: * -> * where
   Screen ::  MyProcedure (Int,Int)
   Uptime ::  MyProcedure Double

instance BinaryQ MyProcedure where
    putQ (Screen) = put (0::Word8)
    putQ (Uptime) = put (1::Word8)

    getQ= do i <- get
             case i :: Word8 of
               0 -> return $ Fmap put Screen 
               1 -> return $ Fmap put Uptime

    interpQ (Screen) = get
    interpQ (Uptime) = get


getScreen :: RemoteMonad MyCommand MyProcedure (Int,Int)
getScreen = procedure Screen

drawShape :: Shape -> RemoteMonad MyCommand MyProcedure ()
drawShape sh = command (Draw sh) 

getUptime :: RemoteMonad MyCommand MyProcedure Double
getUptime = procedure Uptime
