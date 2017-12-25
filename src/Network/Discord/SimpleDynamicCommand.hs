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
module Network.Discord.SimpleDynamicCommand where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Reader.Class

import Network.Discord

import Data.Has
import Data.Text.SimpleTemplate

data CommandInstance = CommandInstance
  { ciCommand :: Text
  , ciArguments :: Vector Text
  , ciChannel :: Snowflake
  }

data CommandParse

instance DiscordAuth m => EventMap CommandParse (DiscordApp m) where
  type Domain CommandParse = Message
  type Codomain CommandParse = CommandInstance
  mapEvent _ (m@Message{messageContent, messageAuthor, messageChannel})
    | userIsBot messageAuthor = mzero
    | V.null split = mzero
    | otherwise =
      let ciCommand = V.head split
          ciArguments = V.tail split
          ciChannel = messageChannel
      in return $ CommandInstance{..}
    where
      split = V.fromList $ T.words messageContent

type CommandMap = Map Text SimpleTemplate

data TextualCommand

instance (DiscordAuth m, MonadReader e (DiscordApp m), Has e CommandMap) => EventMap TextualCommand (DiscordApp m) where
  type Domain TextualCommand = CommandInstance
  type Codomain TextualCommand = ()
  mapEvent _ (CommandInstance{..}) = do
    asks (M.lookup ciCommand . getComponent) >>= \case
      Nothing -> mzero
      Just temp -> do
        let response = renderTemplate temp ciArguments
        void $ doFetch $ CreateMessage ciChannel response Nothing