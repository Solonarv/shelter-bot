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

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Reader.Class

import Network.Discord

import Network.Discord.Command.Parser

import Data.Has
import Data.Text.Template.Simple

type CommandMap = Map Text SimpleTemplate

data TextualCommand

instance (DiscordAuth m, MonadReader e (DiscordApp m), Has e CommandMap) => EventMap TextualCommand (DiscordApp m) where
  type Domain TextualCommand = CommandInstance
  type Codomain TextualCommand = ()
  mapEvent _ (CommandInstance{..}) = do
    asks (M.lookup ciCommand . getComponent) >>= \case
      Nothing -> mzero
      Just temp -> do
        let response = renderTemplate temp (argToText <$> ciArguments)
        guard $ not $ T.null response
        void $ doFetch $ CreateMessage ciChannel response Nothing