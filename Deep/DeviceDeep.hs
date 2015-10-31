{-# LANGUAGE GADTs, KindSignatures, RankNTypes, StandaloneDeriving, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module DeviceDeep where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import DeepCommand
import DeepProcedure
import Expr
import Transport

newtype DeviceMonad a = DeviceMonad (StateT Env IO a)

deriving instance Monad DeviceMonad
deriving instance Applicative DeviceMonad
deriving instance Functor DeviceMonad

runDeviceMonad :: DeviceMonad a -> Env -> IO (a,Env)
runDeviceMonad (DeviceMonad m) env = runStateT m env

data CommandD :: * -> * where
  CommandD :: Command -> CommandD ()

deriving instance Show (CommandD ())

instance Read (Transport CommandD) where
  readsPrec d = readParen (d > 10) $ \ r0 ->
        [ (Transport $ CommandD cmd,r0)
        | (cmd,r1) <- reads r0
        ]

evalCommandD :: CommandD a -> DeviceMonad a
evalCommandD (CommandD cmd) = DeviceMonad $ do
    env <- get
    env' <- liftIO $ evalCommand env cmd
    put env'
    return ()


evalProcedureD :: Procedure a -> DeviceMonad a
evalProcedureD proc = DeviceMonad $ do
    env <- get
    r <- liftIO $ evalProcedure env proc
    return $ r
