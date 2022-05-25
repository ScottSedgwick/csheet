{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module SpellData
  ( module SpellTypes
  , spells
  , spellMap
  ) where

import Data.Map (Map, fromList)
import Data.Text.Lazy (Text)

import SpellTypes
import Cantrips
import FirstLevel
import SecondLevel
import ThirdLevel
import FourthLevel
import FifthLevel

spellMap :: Map Text Spell
spellMap = fromList (map (\s -> (spName s, s)) spells)

spells :: [Spell]
spells = cantrips <> firstLevel <> secondLevel <> thirdLevel <> fourthLevel <> fifthLevel
