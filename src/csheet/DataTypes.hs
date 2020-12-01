{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}
module DataTypes where

import Data.Aeson (FromJSON(..), ToJSON)
import Data.Aeson.BetterErrors (Parse, asIntegral, asRealFloat, asString, eachInArray, fromAesonParser, key, keyOrDefault, toAesonParser')
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
  } deriving (Eq, Show, Generic)
instance ToJSON Attack
asAttack :: Parse e Attack
asAttack = Attack 
         <$> key "weapon" asString
         <*> key "damage" asString
         <*> key "bonus" asString

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
instance ToJSON Spellcasting
asSpellcasting :: Parse e Spellcasting
asSpellcasting = Spellcasting 
         <$> key "spellcastingClass" asString
         <*> key "spellcastingAbility" asString
         <*> key "spellSaveDC" asIntegral
         <*> key "spellAttackBonus" asIntegral
         <*> keyOrDefault "spellsCantrips" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel1" 0 asIntegral
         <*> keyOrDefault "spellsLevel1" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel2" 0 asIntegral
         <*> keyOrDefault "spellsLevel2" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel3" 0 asIntegral
         <*> keyOrDefault "spellsLevel3" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel4" 0 asIntegral
         <*> keyOrDefault "spellsLevel4" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel5" 0 asIntegral
         <*> keyOrDefault "spellsLevel5" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel6" 0 asIntegral
         <*> keyOrDefault "spellsLevel6" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel7" 0 asIntegral
         <*> keyOrDefault "spellsLevel7" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel8" 0 asIntegral
         <*> keyOrDefault "spellsLevel8" [] (eachInArray asString)
         <*> keyOrDefault "slotsLevel9" 0 asIntegral
         <*> keyOrDefault "spellsLevel9" [] (eachInArray asString)

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
  
         <*> key "proficiencies" (eachInArray asString)

         <*> key "acBase" asIntegral
         <*> key "acBonus" asIntegral
         <*> key "initiative" asIntegral
         <*> key "speed" asIntegral
         <*> key "hitPoints" asIntegral
         <*> key "hitDice" asIntegral
  
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
         <*> key "spellcasting" (eachInArray asSpellcasting)
instance FromJSON Character where
  parseJSON = toAesonParser' asCharacter
