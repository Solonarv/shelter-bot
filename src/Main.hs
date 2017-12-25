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

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.Discord.SimpleStaticCommand
import Network.Discord.SimpleDynamicCommand
import Network.Discord.Orphans

import Data.Has
import Data.Text.SimpleTemplate

data Config = Config
  { cfgAuthToken :: String
  , cfgCommandMap :: CommandMap
  }

instance Has Config CommandMap where getComponent = cfgCommandMap

type AppM = ReaderT Config IO

instance DiscordAuth AppM where
  auth = Bot <$> asks cfgAuthToken
  version = return "0.1.0"
  runIO act = do
    cfgAuthToken <- readFile "local/token.txt"
    cfgCommandMap <- readCommandMap
    let cfg = Config {..}
    runReaderT act cfg

readCommandMap :: IO CommandMap
readCommandMap = foldMap readCommand . T.lines <$> T.readFile "local/commands.txt"
  where
    readCommand :: T.Text -> CommandMap
    readCommand l =
      let (cmd, temp) = T.breakOn "=" l
      in M.singleton (T.strip cmd) (parseTemplate $ T.strip $ T.tail temp)

runBot_ :: forall m f. (DiscordAuth m, EventHandler f m) => IO ()
runBot_ = runBot $ Proxy @(m f)

instance EventHandler CommandProcessor AppM

type CommandProcessor = MessageEvent :> CommandParse :> TextualCommand

main = runBot_ @AppM @CommandProcessor