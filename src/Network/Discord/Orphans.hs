{-# LANGUAGE
    MultiParamTypeClasses,
    StandaloneDeriving,
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    UndecidableInstances
    #-}
module Network.Discord.Orphans where

import Network.Discord

import Control.Monad.Reader.Class

instance MonadReader e m => MonadReader e (DiscordApp m) where
  ask = DiscordApp $ \_ _ -> ask
  local f (DiscordApp runEvent) = DiscordApp $ \con evt -> local f $ runEvent con evt