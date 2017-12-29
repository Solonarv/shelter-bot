{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    UndecidableInstances,
    TypeApplications,
    AllowAmbiguousTypes,
    ExplicitForAll,
    ScopedTypeVariables,
    FlexibleContexts
    #-}
module Control.Monad.Environment where

import Data.Maybe
import Data.Proxy

import Control.Monad.Reader
import Control.Monad.State

class Monad m => MonadEnv e m where
    getEnv :: m e
    getEnv = getsEnv id
    getsEnv :: (e -> a) -> m a
    getsEnv f = f <$> getEnv
    {-# MINIMAL getEnv | getsEnv #-}

class MonadEnv e m => MonadEnvMut e m where
    setEnv :: e -> m ()
    setEnv = modifyEnv . const
    modifyEnv :: (e -> e) -> m ()
    modifyEnv f = getEnv >>= setEnv . f
    {-# MINIMAL setEnv | modifyEnv #-}

setOptionalEnv :: MonadEnvMut (Maybe e) m => e -> m ()
setOptionalEnv = setEnv . Just

hasOptionalEnv :: MonadEnv (Maybe e) m => proxy e -> m Bool
hasOptionalEnv (_ :: proxy e) = isJust <$> getEnv @(Maybe e)

hasOptionalEnv' :: forall e m. MonadEnv (Maybe e) m => m Bool
hasOptionalEnv' = hasOptionalEnv $ Proxy @e

deleteOptionalEnv :: MonadEnvMut (Maybe e) m => proxy e -> m ()
deleteOptionalEnv (_ :: proxy e) = setEnv $ Nothing @e

deleteOptionalEnv' :: forall e m. MonadEnvMut (Maybe e) m => m ()
deleteOptionalEnv' = deleteOptionalEnv $ Proxy @e

instance Monad m => MonadEnv e (ReaderT e m) where
    getEnv = ask
    getsEnv = asks

instance Monad m => MonadEnv s (StateT s m) where
    getEnv = get
    getsEnv = gets

instance Monad m => MonadEnvMut s (StateT s m) where
    setEnv = put
    modifyEnv = modify

instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, Monad (t m), MonadEnv e m) => MonadEnv e (t m) where
    getEnv = lift getEnv
    getsEnv = lift . getsEnv

instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, Monad (t m), MonadEnvMut e m) => MonadEnvMut e (t m) where
    setEnv = lift . setEnv
    modifyEnv = lift . modifyEnv