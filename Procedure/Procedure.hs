{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, FlexibleInstances #-}

module Procedure where

data Procedure :: * -> * where
  Temperature ::        Procedure Int
  Toast       :: Int -> Procedure ()

deriving instance Show (Procedure a)

readProcedureReply :: Procedure a -> String -> a
readProcedureReply (Temperature {}) i = read i
readProcedureReply (Toast {})       i = read i
