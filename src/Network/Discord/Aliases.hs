{-# LANGUAGE
    ExplicitNamespaces,
    TypeOperators
    #-}
module Network.Discord.Aliases (
  type MessageEvent
  ) where

import Network.Discord

type MessageEvent = MessageCreateEvent :<>: MessageUpdateEvent