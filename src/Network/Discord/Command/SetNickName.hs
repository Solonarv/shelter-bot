{-# LANGUAGE
    MultiParamTypeClasses,
    UndecidableInstances,
    TypeFamilies
    #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase #-}
module Network.Discord.Command.SetNickName where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import Network.Discord

import Network.Discord.Orphans
import Network.Discord.Command.Parser

import Data.Has

newtype CommandSetNick = CommandSetNick { setNickCommand :: Text }

instance (DiscordAuth m, MonadReader e (DiscordApp m), Has e CommandSetNick) => EventMap CommandSetNick (DiscordApp m) where
  type Domain CommandSetNick = CommandInstance
  type Codomain CommandSetNick = ()
  
  mapEvent _ CommandInstance{ciCommand, ciArguments} = do
    CommandSetNick cmd <- asks getComponent
    guard $ cmd == ciCommand
    argsToUser ciArguments >>= \case
      _ -> return ()