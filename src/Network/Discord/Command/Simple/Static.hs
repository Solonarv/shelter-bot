{-# LANGUAGE
    TypeApplications,
    TypeFamilies,
    DataKinds,
    TypeOperators,
    ScopedTypeVariables,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses,
    AllowAmbiguousTypes
    #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
module Network.Discord.Command.Simple.Static where

import Network.Discord

import GHC.TypeLits
import Data.Proxy
import Data.Text
import Control.Monad

type command :=> reply = Command command :> Reply reply

data Command (a :: Symbol)

instance (DiscordAuth m, KnownSymbol a) => EventMap (Command a) (DiscordApp m) where
  type Domain (Command a) = Message
  type Codomain (Command a) = Message
  
  mapEvent p (m@Message{messageContent, messageAuthor})
    | userIsBot messageAuthor = mzero
    | unpack messageContent == cmdName p = pure m
    | otherwise = mzero
    where
      cmdName :: proxy (Command a) -> String
      cmdName _ = symbolVal $ Proxy @a

data Reply (a :: Symbol)

instance (DiscordAuth m, KnownSymbol a) => EventMap (Reply a) (DiscordApp m) where
  type Domain (Reply a) = Message
  type Codomain (Reply a) = ()
  
  mapEvent p (m@Message{messageChannel}) =
      void $ doFetch $ CreateMessage messageChannel (pack $ replyText p) Nothing
    where
      replyText :: Proxy (Reply a) -> String
      replyText _ = symbolVal $ Proxy @a