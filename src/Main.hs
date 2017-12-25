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
module Main where

import Network.Discord

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Proxy
import GHC.TypeLits

import Network.Discord.SimpleStaticCommand

data Config = Config {
    cfgAuthToken :: String
    }

type AppM = ReaderT Config IO

instance DiscordAuth AppM where
    auth = Bot <$> asks cfgAuthToken
    version = return "0.1.0"
    runIO act = do
        cfgAuthToken <- readFile "local/token.txt"
        let cfg = Config {..}
        runReaderT act cfg

runBot_ :: forall m f. (DiscordAuth m, EventHandler f m) => IO ()
runBot_ = runBot $ Proxy @(m f)

instance EventHandler PingPongApp AppM

type PingPongApp = MessageEvent :> (
       ("!ping" :=> "pong")
  :<>: ("!pong" :=> "ping")
  )

main = runBot_ @AppM @PingPongApp