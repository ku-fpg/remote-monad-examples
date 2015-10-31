{-# LANGUAGE ConstraintKinds, GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, StandaloneDeriving, FlexibleInstances #-}

module DeepCommand where
import Expr
import Binder
import Transport
import GHC.Exts


data Command where
  Say  :: StringExpr              -> Command
  Bind :: Id -> Bindee StringExpr -> Command

deriving instance Show Command
deriving instance Read Command

evalCommand :: Env -> Command -> IO Env
evalCommand env (Say n) = do
    putStrLn $ "Remote: " ++ evalStringExpr env n
    return env
evalCommand env (Bind u p) = do
    env' <- evalBindeeEnv env u p
    return env'
