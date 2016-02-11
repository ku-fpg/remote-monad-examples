{-# LANGUAGE ConstraintKinds, GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, StandaloneDeriving, FlexibleInstances #-}

module DeepProcedure where

import Expr
import Binder
import Transport
import GHC.Exts

-- Remote dereference with type coercion
data Procedure :: * -> * where
  Reply :: StringExpr -> Procedure String

deriving instance Show (Procedure a)

instance Read (Transport Procedure) where
  readsPrec d = readParen (d > 10) $ \ r0 ->
        [ (Transport $ Reply i,r2)
        | ("Reply",r1) <- lex r0
        , (i,r2)      <- reads r1
        ]

evalProcedure :: Env -> Procedure a -> IO a
evalProcedure env (Reply expr) = return $ evalStringExpr env expr

readProcedureReply :: Procedure a -> String -> a
readProcedureReply (Reply {}) i = read i
