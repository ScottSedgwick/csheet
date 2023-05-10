{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DataTypes where

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object)
import GHC.Generics(Generic)

data Alignment = CG | NG | LG | CN | N | LN | CE | NE | LE | None deriving (Eq, Generic)
instance Show Alignment where
  show CG = "Chaotic Good"
  show NG = "Neutral Good"
  show LG = "Lawful Good"
  show CN = "Chaotic Neutral"
  show N = "Neutral"
  show LN = "Lawful Neutral"
  show CE = "Chaotic Evil"
  show NE = "Neutral Evil"
  show LE = "Lawful Evil"
  show None = ""
instance FromJSON Alignment
instance ToJSON Alignment

data Attack = Attack
  { weapon :: String
  , damage :: String
  , bonus :: String
  , notes :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Attack
instance FromJSON Attack 

data SpellLevel = Cantrip | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Unknown deriving (Eq, Show, Generic)
instance ToJSON SpellLevel
instance FromJSON SpellLevel

toSpellLevel :: String -> SpellLevel
toSpellLevel "Cantrip" = Cantrip
toSpellLevel "One" = One
toSpellLevel "Two" = Two
toSpellLevel "Three" = Three
toSpellLevel "Four" = Four
toSpellLevel "Five" = Five
toSpellLevel "Six" = Six
toSpellLevel "Seven" = Seven
toSpellLevel "Eight" = Eight
toSpellLevel "Nine" = Nine
toSpellLevel _ = Unknown

data Spell = Spell
  { prepared :: Bool
  , spellName :: String
  , spellLevel :: SpellLevel
  , spellCastingTime :: String
  , spellComponents :: String
  , spellDuration :: String
  , spellNotes :: String
  , spellPage :: String
  , spellRange :: String
  , spellSave :: String
  , spellSource :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Spell where
  toJSON (Spell prep name lvl ct c d n p r sv src) =
    object  [ "prepared" .= prep
            , "name" .= name
            , "level" .= lvl
            , "castingTime" .= ct
            , "components" .= c
            , "duration" .= d
            , "notes" .= n
            , "page" .= p
            , "range" .= r
            , "save" .= sv
            , "source" .= src
            ]
instance FromJSON Spell 

data Spellcasting = Spellcasting
  { spellcastingClass :: String
  , spellcastingAbility :: String
  , spellSaveDC :: String
  , spellAttackBonus :: String
  , spells :: [Spell]
  , slotsLevel1 :: String
  , slotsLevel2 :: String
  , slotsLevel3 :: String
  , slotsLevel4 :: String
  , slotsLevel5 :: String
  , slotsLevel6 :: String
  , slotsLevel7 :: String
  , slotsLevel8 :: String
  , slotsLevel9 :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Spellcasting
instance FromJSON Spellcasting 

data Cash = Cash {
    copper :: Integer
  , silver :: Integer
  , electrum :: Integer
  , gold :: Integer
  , platinum :: Integer
  , gems :: [String]
} deriving (Eq, Show, Generic)
instance ToJSON Cash
instance FromJSON Cash 

data Backpack = Backpack {
    bagPocket1 :: [String]
  , bagPocket2 :: [String]
  , bagPocket3 :: [String]
  , bagPocket4 :: [String]
  , bagFlapPouch :: [String]
  , bagMiddlePouch :: [String]
  , bagMainPouch :: [String]
  , bagCash :: Cash
  , bagTreasure :: [String] 
  , bagBedroll :: String
  , bagRope :: String
  , bagAmmo :: String
  , bagTorches :: String
} deriving (Eq, Show, Generic)
instance FromJSON Backpack
instance ToJSON Backpack

data BagItem = BagItem {
    biName :: String
  , biQty :: Integer
  , biLbs :: Integer
} deriving (Eq, Show, Generic)
instance FromJSON BagItem
instance ToJSON BagItem

data BagOfHolding = BagOfHolding {
    bohPGs :: [BagItem]
  , bohItems :: [BagItem]
} deriving (Eq, Show, Generic)
instance FromJSON BagOfHolding
instance ToJSON BagOfHolding

newtype PortableHole = PortableHole {
  phItems :: [String]
} deriving (Eq, Show, Generic)
instance FromJSON PortableHole
instance ToJSON PortableHole

data SheetType = SheetTypeStandard
               | SheetTypeIcewindDale
               deriving (Eq, Show, Generic)
instance FromJSON SheetType
instance ToJSON SheetType
toSheetType :: String -> Maybe SheetType
toSheetType "SheetTypeStandard"    = Just SheetTypeStandard
toSheetType "SheetTypeIcewindDale" = Just SheetTypeIcewindDale
toSheetType _ = Nothing

data Character = Character
  { characterName :: String
  , sheetType :: SheetType
  , className :: String
  , subclass :: String
  , level :: Integer
  , background :: String
  , playerName :: String
  , race :: String
  , alignment :: Alignment
  , experience :: String
  , appearance :: String
  , gender :: String
  , faith :: String

  , strength :: Integer
  , dexterity :: Integer
  , constitution :: Integer
  , intelligence :: Integer
  , wisdom :: Integer
  , charisma :: Integer

  , strengthBonus :: Integer
  , dexterityBonus :: Integer
  , constitutionBonus :: Integer
  , intelligenceBonus :: Integer
  , wisdomBonus :: Integer
  , charismaBonus :: Integer

  , inspiration :: Integer
  , proficiencyBonus :: Integer

  , skillAcrobatics :: Double
  , skillAnimalHandling :: Double
  , skillArcana :: Double
  , skillAthletics :: Double
  , skillDeception :: Double
  , skillHistory :: Double
  , skillInsight :: Double
  , skillIntimidation :: Double
  , skillInvestigation :: Double
  , skillMedicine :: Double
  , skillNature :: Double
  , skillPerception :: Double
  , skillPerformance :: Double
  , skillPersuasion :: Double
  , skillReligion :: Double
  , skillSleightOfHand :: Double
  , skillStealth :: Double
  , skillSurvival :: Double

  , bonusAcrobatics :: Integer
  , bonusAnimalHandling :: Integer
  , bonusArcana :: Integer
  , bonusAthletics :: Integer
  , bonusDeception :: Integer
  , bonusHistory :: Integer
  , bonusInsight :: Integer
  , bonusIntimidation :: Integer
  , bonusInvestigation :: Integer
  , bonusMedicine :: Integer
  , bonusNature :: Integer
  , bonusPerception :: Integer
  , bonusPerformance :: Integer
  , bonusPersuasion :: Integer
  , bonusReligion :: Integer
  , bonusSleightOfHand :: Integer
  , bonusStealth :: Integer
  , bonusSurvival :: Integer

  , bonusPassiveInsight :: Integer
  , bonusPassivePerception :: Integer
  , bonusPassiveInvestigation :: Integer
  
  , proficiencies :: [String]

  , acBase :: Integer
  , acBonus :: Integer
  , initiative :: Integer
  , speed :: String
  , hitPoints :: [Integer]
  , tempHitPoints :: Integer
  , hitDice :: String
  
  , attacks :: [Attack]

  , moneyPouch :: Cash
  , backpacks :: [Backpack]
  , bagsOfHolding :: [BagOfHolding]
  , portableHoles :: [PortableHole]
  , equipment :: [String]

  , personalityTraits :: [String]
  , ideals :: [String]
  , bonds :: [String]
  , flaws :: [String]
  , features :: [String]
  -- Page 2
  , age :: Integer
  , height :: String
  , weight :: String
  , eyeColour :: String
  , skinColour :: String
  , hairColour :: String
  , backstory :: [String]
  , allies :: [String]
  , treasure :: [String]
  -- Page 3
  , spellcasting :: Maybe Spellcasting
  } deriving (Eq, Show, Generic)
instance ToJSON Character
instance FromJSON Character 
