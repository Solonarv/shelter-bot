{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances
    #-}
module Data.Has where

class Has env a where
  getComponent :: env -> a

instance Has a a where
  getComponent = id