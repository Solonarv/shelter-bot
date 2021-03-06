{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    MultiParamTypeClasses,
    OverloadedStrings,
    LambdaCase
    #-}
module Network.Discord.User where

import Network.Discord

import Control.Monad.Environment

newtype OwnUser = OwnUser { _ownUser :: User }

ownUser :: (MonadEnvMut (Maybe OwnUser) m, DiscordAuth m) => DiscordApp m User
ownUser = getEnv >>= \case
  Just (OwnUser usr) -> return usr
  Nothing -> do
    usr <- doFetch $ GetCurrentUser
    setEnv $ Just (OwnUser usr)
    return usr
