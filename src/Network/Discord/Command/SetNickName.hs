{-# LANGUAGE
    MultiParamTypeClasses,
    UndecidableInstances,
    TypeFamilies,
    TypeApplications
    #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, LambdaCase #-}
module Network.Discord.Command.SetNickName where

import Data.Text (Text)

import qualified Data.Vector as V

import Control.Exception.Lifted
import Network.HTTP.Req

import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.IO.Class

import Network.Discord hiding (Text)
import qualified Network.Discord as DiscordChannel (Channel(Text))

import Network.Discord.Orphans ()
import Network.Discord.Command.Parser
import Network.Discord.Patch as Patch
import Network.Discord.User

import Control.Monad.Environment

newtype CommandSetNick = CommandSetNick { setNickCommand :: Text }

instance (
  DiscordAuth m, 
  MonadEnv CommandSetNick m, 
  MonadEnvMut (Maybe OwnUser) m,
  MonadBaseControl IO m) 
  => EventMap CommandSetNick (DiscordApp m) where
  type Domain CommandSetNick = CommandInstance
  type Codomain CommandSetNick = ()
  
  mapEvent _ CommandInstance{ciChannel, ciSender, ciCommand, ciArguments} = do
    CommandSetNick cmd <- getEnv
    guard $ cmd == ciCommand
    guild <- case ciChannel of
      DiscordChannel.Text {channelGuild} -> return channelGuild
      _ -> mzero
    (target, newNick) <- case V.length ciArguments of
      1 -> return (ciSender, argToText (V.head ciArguments))
      2 -> (,) <$> retrieveUser (ciArguments V.! 0) (Just $ channelGuild ciChannel) <*> (return $ argToText $ ciArguments V.! 1)
      _ -> mzero
    self <- ownUser
    let req = 
          if (userId target == userId self) 
            then void
              $ doFetch
              $ ModifyOwnNick guild newNick
            else void
              $ doFetch 
              $ ModifyGuildMember guild (userId target) 
              $ emptyPatch {mgmpNickname = Just newNick}
    handle (liftIO . print @HttpException) req
