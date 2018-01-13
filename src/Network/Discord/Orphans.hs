{-# LANGUAGE
    MultiParamTypeClasses,
    StandaloneDeriving,
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    TypeFamilies,
    UndecidableInstances
    #-}
module Network.Discord.Orphans where

import Network.Discord

import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Control
import Control.Monad.Base

instance MonadReader e m => MonadReader e (DiscordApp m) where
  ask = lift ask
  local f (DiscordApp k) = DiscordApp $ \con evt -> local f $ k con evt

instance (MonadBase io m, DiscordAuth m) => MonadBase io (DiscordApp m) where
  liftBase = liftBaseDefault

instance (MonadBaseControl io m, DiscordAuth m) => MonadBaseControl io (DiscordApp m) where
  type StM (DiscordApp m) a = StM m a
  restoreM st = DiscordApp $ \_ _ -> restoreM st
  liftBaseWith f = DiscordApp $ \conn evt -> liftBaseWith (\runInBase -> f $ \act -> runInBase (runEvent act conn evt))