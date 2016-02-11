{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, StandaloneDeriving, FlexibleInstances #-}

module Binder where

import Expr
import Transport

data Bindee :: * -> * where
  Temperature :: Bindee StringExpr

deriving instance Show (Bindee a)

instance Read (Bindee StringExpr) where
  readsPrec d = readParen False $ \ r0 ->
        [ (Temperature,r1)
        | ("Temperature",r1) <- lex r0
        ]

evalBindee :: Bindee a -> IO a
evalBindee (Temperature) = return (Lit "76")

readBindeeReply :: Bindee a -> String -> a
readBindeeReply (Temperature {}) i = read i

evalBindeeEnv :: Env -> Id -> Bindee StringExpr -> IO Env
evalBindeeEnv env u p = do
    r <- evalBindee p
    let v = evalStringExpr env r
    let env' = (u,v) : env
    return env'
