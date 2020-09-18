{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Aeson (FromJSON, ToJSON)
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
  } deriving (Eq, Show, Generic)
instance FromJSON Attack
instance ToJSON Attack

data Spellcasting = Spellcasting
  { spellcastingClass :: String
  , spellcastingAbility :: String
  , spellSaveDC :: Integer
  , spellAttackBonus :: Integer
  , spellsCantrips :: [String]
  , slotsLevel1 :: Integer
  , spellsLevel1 :: [String]
  , slotsLevel2 :: Integer
  , spellsLevel2 :: [String]
  , slotsLevel3 :: Integer
  , spellsLevel3 :: [String]
  , slotsLevel4 :: Integer
  , spellsLevel4 :: [String]
  , slotsLevel5 :: Integer
  , spellsLevel5 :: [String]
  , slotsLevel6 :: Integer
  , spellsLevel6 :: [String]
  , slotsLevel7 :: Integer
  , spellsLevel7 :: [String]
  , slotsLevel8 :: Integer
  , spellsLevel8 :: [String]
  , slotsLevel9 :: Integer
  , spellsLevel9 :: [String]
  } deriving (Eq, Show, Generic)
instance FromJSON Spellcasting
instance ToJSON Spellcasting

data Cash = Cash {
    copper :: Integer
  , silver :: Integer
  , electrum :: Integer
  , gold :: Integer
  , platinum :: Integer
  , gems :: [String]
} deriving (Eq, Show, Generic)
instance FromJSON Cash
instance ToJSON Cash

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

data AbilityGroup = AbilityGroup {
    groupName :: String,
    groupHitDice :: Integer,
    groupCount :: Integer,
    groupAbilities :: [String]
} deriving (Eq, Show, Generic)
instance FromJSON AbilityGroup
instance ToJSON AbilityGroup

data SheetType = SheetTypeStandard
               | SheetTypeIcewindDale
               deriving (Eq, Show, Generic)
instance FromJSON SheetType
instance ToJSON SheetType

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
  , experience :: Integer

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

  , skillAcrobatics :: Integer
  , skillAnimalHandling :: Integer
  , skillArcana :: Integer
  , skillAthletics :: Integer
  , skillDeception :: Integer
  , skillHistory :: Integer
  , skillInsight :: Integer
  , skillIntimidation :: Integer
  , skillInvestigation :: Integer
  , skillMedicine :: Integer
  , skillNature :: Integer
  , skillPerception :: Integer
  , skillPerformance :: Integer
  , skillPersuasion :: Integer
  , skillReligion :: Integer
  , skillSleightOfHand :: Integer
  , skillStealth :: Integer
  , skillSurvival :: Integer
  
  , proficiencies :: [String]

  , acBase :: Integer
  , acBonus :: Integer
  , initiative :: Integer
  , speed :: Integer
  , hitPoints :: Integer
  , hitDice :: Integer
  
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
  , spellcasting :: [Spellcasting]
  , abilities :: Maybe [[AbilityGroup]]
  } deriving (Eq, Show, Generic)
instance FromJSON Character
instance ToJSON Character
