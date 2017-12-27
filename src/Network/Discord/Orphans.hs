{-# LANGUAGE
    MultiParamTypeClasses,
    StandaloneDeriving,
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    UndecidableInstances
    #-}
module Network.Discord.Orphans where

import Network.Discord

import Control.Monad.Trans.Class
import Control.Monad.Reader.Class

instance MonadReader e m => MonadReader e (DiscordApp m) where
  ask = lift ask
  local f (DiscordApp k) = DiscordApp $ \con evt -> local f $ k con evt

instance MonadTrans DiscordApp where
  lift act = DiscordApp $ \_ _ -> act