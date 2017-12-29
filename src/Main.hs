{-# LANGUAGE
    TypeApplications,
    TypeFamilies,
    DataKinds,
    TypeOperators,
    ScopedTypeVariables,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses,
    GeneralizedNewtypeDeriving,
    UndecidableInstances
    #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
module Main where

import Network.Discord

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State as St
import Data.Proxy

import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.Base
import Control.Monad.Trans.Control

import Network.Discord.Orphans ()
import Network.Discord.Aliases

import Network.Discord.User

import Network.Discord.Command.Parser
import Network.Discord.Command.Simple.Dynamic
import Network.Discord.Command.SetNickName

import Control.Monad.Environment
import Data.Text.Template.Simple

data Config = Config
  { cfgAuthToken :: String
  , cfgCommandMap :: CommandMap
  , cfgNickComand :: CommandSetNick
  , cfgOwnUser :: OwnUser
  }

type AppStack = StateT Config IO

newtype AppM a = AppM { runAppM :: AppStack a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadState Config
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadBase IO
    )

instance MonadBaseControl IO AppM where
  type StM AppM a = StM AppStack a
  restoreM st = AppM $ restoreM st
  liftBaseWith f = AppM $ liftBaseWith $ \rio -> f (rio . runAppM)

instance MonadEnv CommandMap AppM where
  getEnv = gets cfgCommandMap

instance MonadEnv CommandSetNick AppM where
  getEnv = gets cfgNickComand

instance MonadEnv OwnUser AppM where
  getEnv = gets cfgOwnUser

instance MonadEnvMut OwnUser AppM where
  modifyEnv f = St.modify $ \cfg -> cfg { cfgOwnUser = f (cfgOwnUser cfg)}

instance DiscordAuth AppM where
  auth = Bot <$> gets cfgAuthToken
  version = return "0.1.0"
  runIO (AppM act) = do
    cfgAuthToken <- readFile "local/token.txt"
    cfgCommandMap <- readCommandMap
    let cfgNickComand = CommandSetNick "!nick"
    let cfgOwnUser = undefined
    let cfg = Config {..}
    evalStateT act cfg

readCommandMap :: IO CommandMap
readCommandMap = foldMap readCommand . T.lines <$> T.readFile "local/commands.txt"
  where
    readCommand :: T.Text -> CommandMap
    readCommand l =
      let (cmd, temp) = T.breakOn "=" l
      in M.singleton (T.strip cmd) (parseTemplate $ T.strip $ T.tail temp)

data PerformSetup

instance (MonadEnvMut OwnUser m, DiscordAuth m) => EventMap PerformSetup (DiscordApp m) where
  type Domain PerformSetup = Init
  type Codomain PerformSetup = ()
  mapEvent _ (Init _ usr _ _ _) = setEnv $ OwnUser usr 

instance EventHandler RunApp AppM

type ReadyHandler = ReadyEvent :> PerformSetup

type CommandProcessor = MessageEvent :> CommandParse
  :> (   TextualCommand
    :<>: CommandSetNick
  ) 

type RunApp = ReadyHandler :<>: CommandProcessor

main :: IO ()
main = runBot $ Proxy @(AppM RunApp)
