{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module RPacket
  (RPacket,execRPacket)
where

import Control.Monad

import RCommand
import RProcedure

data RPacket = Packet [RCommand] (Maybe RProcedure)
               deriving (Read, Show)

execRPacket :: RPacket -> IO String
execRPacket (Packet cs p) = do
        mapM_ execRCommand cs
        case p of
         Just p' -> execRProcedure p'
         _ -> return $ show ()
