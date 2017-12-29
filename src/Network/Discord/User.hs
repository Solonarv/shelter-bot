{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    MultiParamTypeClasses,
    OverloadedStrings
    #-}
module Network.Discord.User where

import Data.Text (Text)

import Data.Aeson
import Data.Hashable

import Network.Discord
import Network.Discord.Rest

import Control.Monad.Environment

newtype OwnUser = OwnUser { _ownUser :: User }

ownUser :: MonadEnv OwnUser m => m User
ownUser = getsEnv _ownUser

