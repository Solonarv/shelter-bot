{-# LANGUAGE
    TypeApplications,
    TypeFamilies,
    DataKinds,
    TypeOperators,
    ScopedTypeVariables,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses,
    GeneralizedNewtypeDeriving
    #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
module Main where

import Network.Discord

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Proxy

import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.Discord.Orphans ()
import Network.Discord.Aliases

import Network.Discord.Command.Parser
import Network.Discord.Command.Simple.Dynamic

import Control.Monad.Environment
import Data.Text.Template.Simple

data Config = Config
  { cfgAuthToken :: String
  , cfgCommandMap :: CommandMap
  }

newtype AppM a = AppM { runAppM :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, Alternative, MonadPlus)

instance MonadEnv CommandMap AppM where
  getEnv = asks cfgCommandMap

instance DiscordAuth AppM where
  auth = Bot <$> asks cfgAuthToken
  version = return "0.1.0"
  runIO (AppM act) = do
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

instance EventHandler CommandProcessor AppM

type CommandProcessor = MessageEvent :> CommandParse :> TextualCommand

main :: IO ()
main = runBot $ Proxy @(AppM CommandProcessor)