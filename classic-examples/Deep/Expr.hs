{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, StandaloneDeriving, FlexibleInstances #-}

module Expr where

import Data.Monoid
import Data.String

type Id = Int

data StringExpr = Var Id
                | Lit String
                | Append StringExpr StringExpr

deriving instance Show StringExpr
deriving instance Read StringExpr

instance IsString StringExpr where
  fromString = Lit

instance Monoid StringExpr where
  mempty = Lit ""
  mappend = Append

type Env = [(Id,String)]

evalStringExpr :: Env -> StringExpr -> String
evalStringExpr env (Var u) = case lookup u env of
                        Nothing -> error $ "can not find " ++ show u
                        Just v -> v
evalStringExpr env (Lit i) = i
evalStringExpr env (Append e1 e2) = evalStringExpr env e1 <> evalStringExpr env e2
