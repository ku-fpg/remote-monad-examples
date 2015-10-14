{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Packet where

import Control.Monad

import Command
import Procedure

data Packet a = Packet [Command] (Maybe (Procedure a))
                deriving Show
