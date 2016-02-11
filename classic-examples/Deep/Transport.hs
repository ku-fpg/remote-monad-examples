{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, StandaloneDeriving, FlexibleInstances #-}

module Transport where
        
data Transport (m :: * -> *)
  = forall a . (Show a) => Transport (m a)


evalTransport :: (forall a . Show a => m a -> IO a) 
              -> Transport m
              -> IO String
evalTransport eval (Transport m) = do
        r <- eval m
        return (show r)
        
        
addTransport :: (forall a . t a -> Transport t) -> t a -> Transport t
addTransport f t = f t

--  = forall a . (Show (m a),Show a) => Transport (m a)
