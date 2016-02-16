{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Remote.Monad
import Data.Aeson
import Data.Text

data Shape :: * where
   Square    :: Int ->        Shape
   Diamond   ::               Shape
   Rectangle :: Int -> Int -> Shape
  deriving (Show)

instance ToJSON Shape where
  toJSON (Square s) = object [ ("shape"::Text) .= ("square" ::Text)
                             , ("length"::Text) .= s]
  toJSON (Diamond) = object [ ("shape" :: Text) .= ("diamond"::Text)]
  toJSON (Rectangle w h) = object [ ("shape"::Text) .= ("rectangle"::Text)
                                 , ("width"::Text) .= w 
                                 , ("height"::Text) .= h
                                 ]

instance FromJSON Shape where
   parseJSON = withObject "Shape parser" $ \o -> do
        shape <- o .: ("shape"::Text)
        case (shape::Text) of
          "square"    -> Square <$> o .: ("length"::Text)
          "diamond"   -> return Diamond
          "rectangle" -> Rectangle <$> o .: ("width"::Text)
                                   <*> o .: ("height"::Text)

square :: Shape
square = Square 4 

diamond :: Shape
diamond = Diamond

rectangle :: Shape
rectangle = Rectangle 5 3

data MyCommand :: * where
   Color   :: String  -> MyCommand
   Draw    :: Shape   -> MyCommand

instance ToJSON MyCommand where
    toJSON (Color s) = object [ ("method"::Text) .= ("color" ::Text)
                              , ("params"::Text) .= s
                              ]
    toJSON (Draw s) = object [ ("method"::Text) .= ("draw"::Text)
                             , ("params"::Text) .= s
                             ]


data MyProcedure :: * -> * where
   Screen ::  MyProcedure (Int,Int)
   Uptime ::  MyProcedure Double

instance ToJSON (MyProcedure a) where
     toJSON (Screen) = object [ ("method"::Text) .= ("screen"::Text)]                    
     toJSON (Uptime) = object [ ("method"::Text) .= ("uptime"::Text) ]                    

getScreen :: RemoteMonad MyCommand MyProcedure (Int,Int)
getScreen = procedure Screen

drawShape :: Shape -> RemoteMonad MyCommand MyProcedure ()
drawShape sh = command (Draw sh) 

getUptime :: RemoteMonad MyCommand MyProcedure Double
getUptime = procedure Uptime
