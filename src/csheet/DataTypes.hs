{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
module DataTypes where

import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object)
import Data.Aeson.BetterErrors (Parse, asIntegral, asRealFloat, asString, eachInArray, fromAesonParser, key, keyOrDefault, keyMay, toAesonParser', asBool)
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
asAlignment :: Parse e Alignment
asAlignment = fromAesonParser

data Attack = Attack
  { weapon :: String
  , damage :: String
  , bonus :: String
  , notes :: String
  } deriving (Eq, Show, Generic)
instance ToJSON Attack
asAttack :: Parse e Attack
asAttack = Attack 
         <$> key "weapon" asString
         <*> key "damage" asString
         <*> key "bonus" asString
         <*> keyOrDefault "notes" "" asString

data SpellLevel = Cantrip | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Unknown deriving (Eq, Show, Generic)
instance ToJSON SpellLevel
asSpellLevel :: Parse e SpellLevel
asSpellLevel = toSpellLevel <$> asString

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
asSpell :: Parse e Spell
asSpell = Spell
        <$> key "prepared" asBool 
        <*> key "name" asString
        <*> key "level" asSpellLevel
        <*> keyOrDefault "castingTime" "" asString
        <*> keyOrDefault "components" "" asString
        <*> keyOrDefault "duration" "" asString
        <*> keyOrDefault "notes" "" asString
        <*> keyOrDefault "page" "" asString
        <*> keyOrDefault "range" "" asString
        <*> keyOrDefault "save" "" asString
        <*> keyOrDefault "source" "" asString

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
asSpellcasting :: Parse e Spellcasting
asSpellcasting = Spellcasting 
         <$> key "spellcastingClass" asString
         <*> key "spellcastingAbility" asString
         <*> key "spellSaveDC" asString
         <*> key "spellAttackBonus" asString
         <*> keyOrDefault "spells" [] (eachInArray asSpell)
         <*> keyOrDefault "slotsLevel1" "0" asString
         <*> keyOrDefault "slotsLevel2" "0" asString
         <*> keyOrDefault "slotsLevel3" "0" asString
         <*> keyOrDefault "slotsLevel4" "0" asString
         <*> keyOrDefault "slotsLevel5" "0" asString
         <*> keyOrDefault "slotsLevel6" "0" asString
         <*> keyOrDefault "slotsLevel7" "0" asString
         <*> keyOrDefault "slotsLevel8" "0" asString
         <*> keyOrDefault "slotsLevel9" "0" asString

data Cash = Cash {
    copper :: Integer
  , silver :: Integer
  , electrum :: Integer
  , gold :: Integer
  , platinum :: Integer
  , gems :: [String]
} deriving (Eq, Show, Generic)
instance ToJSON Cash
asCash :: Parse e Cash
asCash = Cash 
         <$> keyOrDefault "copper" 0 asIntegral
         <*> keyOrDefault "silver" 0 asIntegral
         <*> keyOrDefault "electrum" 0 asIntegral
         <*> keyOrDefault "gold" 0 asIntegral
         <*> keyOrDefault "platinum" 0 asIntegral
         <*> keyOrDefault "gems" [] (eachInArray asString)
instance FromJSON Cash where
  parseJSON = toAesonParser' asCash

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
asBackpack :: Parse e Backpack
asBackpack = fromAesonParser

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
asBagOfHolding :: Parse e BagOfHolding
asBagOfHolding = fromAesonParser

newtype PortableHole = PortableHole {
  phItems :: [String]
} deriving (Eq, Show, Generic)
instance FromJSON PortableHole
instance ToJSON PortableHole
asPortableHole :: Parse e PortableHole
asPortableHole = fromAesonParser

data SheetType = SheetTypeStandard
               | SheetTypeIcewindDale
               deriving (Eq, Show, Generic)
instance FromJSON SheetType
instance ToJSON SheetType
asSheetType :: Parse e SheetType
asSheetType = fromAesonParser
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
asCharacter :: Parse e Character
asCharacter = Character 
         <$> key "characterName" asString
         <*> keyOrDefault "sheetType" SheetTypeStandard asSheetType
         <*> key "className" asString
         <*> key "subclass" asString
         <*> key "level" asIntegral
         <*> key "background" asString
         <*> key "playerName" asString
         <*> key "race" asString
         <*> key "alignment" asAlignment
         <*> key "experience" asString
         <*> keyOrDefault "appearance" "" asString
         <*> keyOrDefault "gender" "" asString
         <*> keyOrDefault "faith" "" asString

         <*> key "strength" asIntegral
         <*> key "dexterity" asIntegral
         <*> key "constitution" asIntegral
         <*> key "intelligence" asIntegral
         <*> key "wisdom" asIntegral
         <*> key "charisma" asIntegral

         <*> key "strengthBonus" asIntegral
         <*> key "dexterityBonus" asIntegral
         <*> key "constitutionBonus" asIntegral
         <*> key "intelligenceBonus" asIntegral
         <*> key "wisdomBonus" asIntegral
         <*> key "charismaBonus" asIntegral

         <*> key "inspiration" asIntegral
         <*> key "proficiencyBonus" asIntegral

         <*> key "skillAcrobatics" asRealFloat
         <*> key "skillAnimalHandling" asRealFloat
         <*> key "skillArcana" asRealFloat
         <*> key "skillAthletics" asRealFloat
         <*> key "skillDeception" asRealFloat
         <*> key "skillHistory" asRealFloat
         <*> key "skillInsight" asRealFloat
         <*> key "skillIntimidation" asRealFloat
         <*> key "skillInvestigation" asRealFloat
         <*> key "skillMedicine" asRealFloat
         <*> key "skillNature" asRealFloat
         <*> key "skillPerception" asRealFloat
         <*> key "skillPerformance" asRealFloat
         <*> key "skillPersuasion" asRealFloat
         <*> key "skillReligion" asRealFloat
         <*> key "skillSleightOfHand" asRealFloat
         <*> key "skillStealth" asRealFloat
         <*> key "skillSurvival" asRealFloat

         <*> keyOrDefault "bonusAcrobatics" 0 asIntegral
         <*> keyOrDefault "bonusAnimalHandling" 0 asIntegral
         <*> keyOrDefault "bonusArcana" 0 asIntegral
         <*> keyOrDefault "bonusAthletics" 0 asIntegral
         <*> keyOrDefault "bonusDeception" 0 asIntegral
         <*> keyOrDefault "bonusHistory" 0 asIntegral
         <*> keyOrDefault "bonusInsight" 0 asIntegral
         <*> keyOrDefault "bonusIntimidation" 0 asIntegral
         <*> keyOrDefault "bonusInvestigation" 0 asIntegral
         <*> keyOrDefault "bonusMedicine" 0 asIntegral
         <*> keyOrDefault "bonusNature" 0 asIntegral
         <*> keyOrDefault "bonusPerception" 0 asIntegral
         <*> keyOrDefault "bonusPerformance" 0 asIntegral
         <*> keyOrDefault "bonusPersuasion" 0 asIntegral
         <*> keyOrDefault "bonusReligion" 0 asIntegral
         <*> keyOrDefault "bonusSleightOfHand" 0 asIntegral
         <*> keyOrDefault "bonusStealth" 0 asIntegral
         <*> keyOrDefault "bonusSurvival" 0 asIntegral

         <*> keyOrDefault "bonusPassiveInsight" 0 asIntegral
         <*> keyOrDefault "bonusPassivePerception" 0 asIntegral
         <*> keyOrDefault "bonusPassiveInvestigation" 0 asIntegral
  
         <*> key "proficiencies" (eachInArray asString)

         <*> key "acBase" asIntegral
         <*> key "acBonus" asIntegral
         <*> key "initiative" asIntegral
         <*> key "speed" asString
         <*> key "hitPoints" (eachInArray asIntegral)
         <*> key "tempHitPoints" asIntegral
         <*> key "hitDice" asString
  
         <*> key "attacks" (eachInArray asAttack)

         <*> key "moneyPouch" asCash
         <*> key "backpacks" (eachInArray asBackpack)
         <*> key "bagsOfHolding" (eachInArray asBagOfHolding)
         <*> key "portableHoles" (eachInArray asPortableHole)
         <*> key "equipment" (eachInArray asString)

         <*> key "personalityTraits" (eachInArray asString)
         <*> key "ideals" (eachInArray asString)
         <*> key "bonds" (eachInArray asString)
         <*> key "flaws" (eachInArray asString)
         <*> key "features" (eachInArray asString)
  -- Page 2
         <*> key "age" asIntegral
         <*> key "height" asString
         <*> key "weight" asString
         <*> key "eyeColour" asString
         <*> key "skinColour" asString
         <*> key "hairColour" asString
         <*> key "backstory" (eachInArray asString)
         <*> key "allies" (eachInArray asString)
         <*> key "treasure" (eachInArray asString)
  -- Page 3
         <*> keyMay "spellcasting" asSpellcasting
instance FromJSON Character where
  parseJSON = toAesonParser' asCharacter
