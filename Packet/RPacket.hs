{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module RPacket
  (RPacket,execRPacket)
where

import Control.Monad

import RCommand
import RProcedure

data RPacket = Packet [RCommand] RProcedure
               deriving Read

execRPacket :: RPacket -> IO String
execRPacket (Packet cs p) = do
        mapM_ execRCommand cs
	execRProcedure p
