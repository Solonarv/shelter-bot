{-# LANGUAGE OverloadedStrings #-}
module Data.Text.SimpleTemplate where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Char

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Monoid

newtype SimpleTemplate = SimpleTemplate { templateElems :: Vector TemplateElement }
  deriving (Eq, Ord, Show)

data TemplateElement = ElemText Text | ElemArgument Int | ElemPositional
  deriving (Eq, Ord, Show)

showTemplateElem :: TemplateElement -> Text
showTemplateElem (ElemText txt) = T.replace "$" "$$" txt
showTemplateElem (ElemArgument i) = T.pack $ '$' : show i
showTemplateElem ElemPositional = "$_"

showTemplate :: SimpleTemplate -> Text
showTemplate (SimpleTemplate v) = foldMap showTemplateElem v

instance Monoid SimpleTemplate where
  mempty = SimpleTemplate V.empty
  SimpleTemplate v0 `mappend` SimpleTemplate v1 = simplify $ SimpleTemplate $ v0 V.++ v1
  mconcat = simplify . SimpleTemplate . mconcat . map templateElems

simplify :: SimpleTemplate -> SimpleTemplate
simplify (SimpleTemplate elems) = SimpleTemplate $ V.foldl' go V.empty elems
  where
    go :: Vector TemplateElement -> TemplateElement -> Vector TemplateElement
    go acc x = case x of
      ElemText txt -> if V.null acc
        then V.singleton (ElemText txt)
        else case V.last acc of
          ElemText prev -> (V.init acc) `V.snoc` ElemText (prev <> txt)
          _ -> acc `V.snoc` x
      _ -> acc `V.snoc` x

parseTemplate :: Text -> SimpleTemplate
parseTemplate = SimpleTemplate . snd . T.foldl go (Nothing, V.empty)
  where
    go (Nothing, acc) char = case char of
      '$' -> (Just "", acc)
      _ -> (Nothing, append acc (T.singleton char))
    go (Just st, acc) char
      | T.null st && char == '_' = (Nothing, acc `V.snoc` ElemPositional)
      | T.null st && char == '$' = (Nothing, append acc "$")
      | isDigit char = (Just $ st `T.snoc` char, acc)
      | T.null st = (Nothing, append acc "$")
      | otherwise = go (Nothing, acc `V.snoc` ElemArgument (read (T.unpack st) - 1)) char
    append :: Vector TemplateElement -> Text -> Vector TemplateElement
    append v t = if V.null v
      then V.singleton (ElemText t)
      else case V.last v of
        ElemText txt -> V.init v `V.snoc` ElemText (txt <> t)
        _ -> v `V.snoc` ElemText t

renderTemplate :: SimpleTemplate -> Vector Text -> Text
renderTemplate (SimpleTemplate es) args = snd $ V.foldl' go (0, "") es
  where
    go (ix, acc) (ElemText txt) = (ix, acc <> txt)
    go (ix, acc) (ElemArgument x) = (ix, acc <> arg x)
    go (ix, acc) (ElemPositional) = (ix + 1, acc <> arg ix)
    arg i = case args V.!? i of
      Nothing -> ""
      Just t -> t