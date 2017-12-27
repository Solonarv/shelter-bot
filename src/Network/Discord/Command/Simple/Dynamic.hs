{-# LANGUAGE
    TypeApplications,
    TypeFamilies,
    DataKinds,
    TypeOperators,
    ScopedTypeVariables,
    TypeSynonymInstances,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    MultiParamTypeClasses
    #-}
{-# LANGUAGE    
    OverloadedStrings,
    NamedFieldPuns,
    RecordWildCards,
    LambdaCase
    #-}
module Network.Discord.Command.Simple.Dynamic where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad

import Network.Discord

import Network.Discord.Command.Parser
import Network.Discord.Orphans ()

import Control.Monad.Environment
import Data.Text.Template.Simple

type CommandMap = Map Text SimpleTemplate

data TextualCommand

instance (DiscordAuth m, MonadEnv CommandMap m) => EventMap TextualCommand (DiscordApp m) where
  type Domain TextualCommand = CommandInstance
  type Codomain TextualCommand = ()
  mapEvent _ (CommandInstance{..}) = do
    M.lookup ciCommand <$> getEnv >>= \case
      Nothing -> mzero
      Just temp -> do
        let response = renderTemplate temp (argToText <$> ciArguments)
        guard $ not $ T.null response
        void $ doFetch $ CreateMessage (channelId ciChannel) response Nothing