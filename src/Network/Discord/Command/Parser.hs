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
    NamedFieldPuns,
    RecordWildCards,
    OverloadedStrings
    #-}
module Network.Discord.Command.Parser where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.IO.Class

import Network.Discord

import Data.Char (isSpace)

import Data.Monoid

import Text.Parsec
import Text.Parsec.Text

data CommandArg = ArgText Text | ArgSnowflake (Maybe SnowflakeType) Snowflake

argToText :: CommandArg -> Text
argToText (ArgText tx) = tx
argToText (ArgSnowflake ty flake) = case ty of
  Nothing -> shown
  Just (SnowflakeUser isNick) -> (if isNick then "<@" else "<@!") <> shown <> ">"
  Just SnowflakeRole -> "<@&" <> shown <> ">"
  Just SnowflakeChannel -> "<#" <> shown <> ">"
  where shown = T.pack $ show flake

data SnowflakeType = SnowflakeUser Bool | SnowflakeChannel | SnowflakeRole

parseText :: Parser Text
parseText = quotedText <|> unquotedText
  where
    quotedText = quote *> (T.pack <$> many (nonQuote)) <* quote
    quote = char '"' <?> "quote"
    nonQuote = ('"' <$ string "\\\"") <|> noneOf "\""
    unquotedText = T.pack <$> many1 (satisfy $ not . isSpace)

parseArg :: Parser CommandArg
parseArg = parseSnowflakeArg <|> ArgText <$> parseText
  where
    parseSnowflakeArg = explicitSnowflake <|> userMention <|> roleMention <|> channelMention
    snowflake = Snowflake . read <$> many1 digit
    explicitSnowflake = ArgSnowflake Nothing                 <$> snowflake
    roleMention       = ArgSnowflake (Just SnowflakeRole)    <$> (string "<@&"                        *> snowflake <* string ">")
    channelMention    = ArgSnowflake (Just SnowflakeChannel) <$> (string "<#"                         *> snowflake <* string ">")
    userMention       = do
      string "<@"
      usingNick <- option False (True <$ char '!')
      flake <- snowflake
      string ">"
      return $ ArgSnowflake (Just (SnowflakeUser usingNick)) flake

parseCommand :: Parser (Text, Vector CommandArg)
parseCommand = (,) <$> parseText <* spaces
                   <*> (V.fromList <$> many (parseArg <* spaces))

data CommandInstance = CommandInstance
  { ciCommand :: Text
  , ciOriginalCommand :: Text
  , ciArguments :: Vector CommandArg
  , ciChannel :: Snowflake
  , ciSender :: User
  }

data CommandParse

instance DiscordAuth m => EventMap CommandParse (DiscordApp m) where
  type Domain CommandParse = Message
  type Codomain CommandParse = CommandInstance
  mapEvent _ Message{messageContent, messageAuthor, messageChannel}
    | userIsBot messageAuthor = mzero
    | otherwise =
      let ciOriginalCommand = messageContent
          ciChannel = messageChannel
          ciSender = messageAuthor  
      in case parse parseCommand "<command>" messageContent of
        Right (ciCommand, ciArguments) -> return $ CommandInstance{..}
        Left err -> liftIO (print err) >> mzero
        
retrieveUser :: DiscordAuth m => CommandArg -> Maybe Snowflake -> DiscordApp m User
retrieveUser (ArgSnowflake ty fl) _ = do
  snowflake <- case ty of
    Nothing -> pure fl
    Just (SnowflakeUser _) -> pure fl
    _ -> mzero
  doFetch $ GetUser snowflake
retrieveUser (ArgText _) Nothing = mzero
retrieveUser (ArgText nm) (Just srv) = mzero
  