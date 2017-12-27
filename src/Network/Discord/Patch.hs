{-# LANGUAGE RecordWildCards, OverloadedStrings, NoMonomorphismRestriction #-}
module Network.Discord.Patch where

import Data.Text (Text)

import Data.Maybe

import Data.Aeson

import Network.Discord

class Patch a where
    emptyPatch :: a

data ModifyGuildMemberPatch = ModifyGuildMemberPatch
  { mgmpNickname :: Maybe Text
  , mgmpRoles :: Maybe [Snowflake]
  , mgmpMute :: Maybe Bool
  , mgmpDeaf :: Maybe Bool
  , mgmpVoiceChannel :: Maybe Snowflake
  } deriving (Eq, Show)

instance ToJSON ModifyGuildMemberPatch where
  toJSON ModifyGuildMemberPatch{..} = object $ catMaybes
    [ kv "nick"       <$> mgmpNickname
    , kv "roles"      <$> mgmpRoles
    , kv "mute"       <$> mgmpMute
    , kv "deaf"       <$> mgmpDeaf
    , kv "channel_id" <$> mgmpVoiceChannel
    ]
    where
      kv = (.=)
  
instance Patch ModifyGuildMemberPatch where
  emptyPatch = ModifyGuildMemberPatch Nothing Nothing Nothing Nothing Nothing
  