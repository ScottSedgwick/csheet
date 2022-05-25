{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module SpellTypes where

import Data.Text.Lazy (Text)

data Spell = Spell
  { spName :: Text
  , spLevel :: Int
  , spType :: SpellType
  , spRitual :: Bool
  , spTime :: Text
  , spRange :: Text
  , spComponents :: Text
  , spDuration :: Text
  , spDescription :: Text
  , spHigher :: Maybe Text
  } deriving stock Show

data SpellType 
  = Abjuration
  | Conjuration
  | Divination
  | Enchantment
  | Evocation
  | Illusion
  | Necromancy
  | Transmutation
  deriving stock (Show, Eq)