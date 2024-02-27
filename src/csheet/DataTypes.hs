{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DataTypes where

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:), (.:?), (.!=), Value(..), object)
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

data SpellLevel = Cantrip | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | SpellLevelUnknown deriving (Eq, Show, Ord, Generic)
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
toSpellLevel _ = SpellLevelUnknown

data SpellType
  = Abjuration
  | Conjuration
  | Divination
  | Enchantment
  | Evocation
  | Illusion
  | Necromancy
  | Transmutation
  | SpellTypeUnknown
  deriving (Show, Eq, Generic)
instance ToJSON SpellType
instance FromJSON SpellType

data Spell = Spell
  { spName :: String
  , spLevel :: SpellLevel
  , spType :: SpellType
  , spRitual :: Bool
  , spTime :: String
  , spRange :: String
  , spComponents :: String
  , spDuration :: String
  , spDescription :: String
  , spHigher :: Maybe String
  } deriving (Show, Eq, Generic)
instance ToJSON Spell
instance FromJSON Spell

data KnownSpell = KnownSpell
  { prepared :: Bool
  , spellName :: String
  } deriving (Eq, Show, Generic)
instance ToJSON KnownSpell
instance FromJSON KnownSpell

data Spellcasting = Spellcasting
  { spellcastingClass :: String
  , spellcastingAbility :: String
  , spellSaveDC :: String
  , spellAttackBonus :: String
  , knownSpells :: [KnownSpell]
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

data ActionType = Action
                | Bonus
                | Reaction
                | NoAction
                deriving (Eq, Generic)
instance ToJSON ActionType
instance FromJSON ActionType
instance Show ActionType where
  show Action   = " [A]"
  show Bonus    = " [B]"
  show Reaction = " [R]"
  show NoAction = ""

data Feature = Feature
  { action :: ActionType
  , infotext :: String
  , comment :: String
  } deriving (Eq, Generic)
instance ToJSON Feature
instance FromJSON Feature
instance Show Feature where
  show f = (infotext f) <> show (action f)

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

  , strengthProf :: Integer
  , dexterityProf :: Integer
  , constitutionProf :: Integer
  , intelligenceProf :: Integer
  , wisdomProf :: Integer
  , charismaProf :: Integer
  , profAdditionalBonus :: Integer

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

  , advantageAcrobatics :: Double
  , advantageAnimalHandling :: Double
  , advantageArcana :: Double
  , advantageAthletics :: Double
  , advantageDeception :: Double
  , advantageHistory :: Double
  , advantageInsight :: Double
  , advantageIntimidation :: Double
  , advantageInvestigation :: Double
  , advantageMedicine :: Double
  , advantageNature :: Double
  , advantagePerception :: Double
  , advantagePerformance :: Double
  , advantagePersuasion :: Double
  , advantageReligion :: Double
  , advantageSleightOfHand :: Double
  , advantageStealth :: Double
  , advantageSurvival :: Double

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
  , features :: [Feature]
  -- Page 2
  , age :: String
  , height :: String
  , weight :: String
  , eyeColour :: String
  , skinColour :: String
  , hairColour :: String
  , backstory :: [String]
  , allies :: [String]
  , faction :: String
  , treasure :: [String]
  -- Page 3
  , spellcasting :: [Spellcasting]
  } deriving (Eq, Show, Generic)
instance ToJSON Character
instance FromJSON Character
