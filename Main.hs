{-# LANGUAGE
    TypeApplications,
    TypeFamilies,
    DataKinds,
    TypeOperators,
    ScopedTypeVariables,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses
    #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import Network.Discord

import Pipes hiding (Proxy)
import Data.Text
import Control.Monad.IO.Class
import Data.Proxy
import GHC.TypeLits

instance DiscordAuth IO where
    auth = Bot <$> readFile "local/token.txt"
    version = return "0.1.0"
    runIO = id

data Command (a :: Symbol)

instance KnownSymbol a => EventMap (Command a) (DiscordApp IO) where
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

instance KnownSymbol a => EventMap (Reply a) (DiscordApp IO) where
    type Domain (Reply a) = Message
    type Codomain (Reply a) = ()
    
    mapEvent p (m@Message{messageChannel}) =
        void $ doFetch $ CreateMessage messageChannel (pack $ replyText p) Nothing
      where
        replyText :: Proxy (Reply a) -> String
        replyText _ = symbolVal $ Proxy @a


type PingPong = 
  (
    (MessageCreateEvent :<>: MessageUpdateEvent) :>
      (    (Command "ping" :> Reply "pong")
      :<>: (Command "pong" :> Reply "ping")
      )
   )

instance EventHandler PingPong IO

main = runBot $ Proxy @(IO PingPong)