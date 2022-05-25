{-# LANGUAGE OverloadedStrings #-}
module DataMaps (loadPdfData, listUnparsedFields) where

import DataTypes
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import qualified SpellData as SD
import Text.Read (readMaybe)

type CharSetter = Character -> T.Text -> Character
type DataMap = (T.Text, CharSetter)

loadPdfData :: M.Map T.Text T.Text -> Character
loadPdfData m = c3
  where
    c1 = foldr (loadPdfField m) emptyCharacter dataMaps
    c2 = loadSpells m c1
    c3 = loadWpns m c2

listUnparsedFields :: M.Map T.Text T.Text -> M.Map T.Text T.Text
listUnparsedFields m = foldr (\a b -> if a `M.member` b then M.delete a b else b) m (map fst dataMaps)

loadWpns :: M.Map T.Text T.Text -> Character -> Character
loadWpns m c = c { attacks = loadAttacks m }

loadAttacks :: M.Map T.Text T.Text -> [Attack]
loadAttacks m = mapMaybe (loadAttack m) [0..100]

loadAttack :: M.Map T.Text T.Text -> Int -> Maybe Attack
loadAttack m n = 
  Attack <$> look wpn
         <*> look ("Wpn" <> show n <> " Damage")
         <*> look ("Wpn" <> show n <> " AtkBonus")
         <*> look ("Wpn Notes " <> show n)
  where
    look k = T.unpack <$> M.lookup (T.pack k) m
    wpn = if n == 1 then "Wpn Name" else "Wpn Name " <> show n

loadSpells :: M.Map T.Text T.Text -> Character -> Character
loadSpells m c = c { spellcasting = loadSpellcasting m }

loadSpellcasting :: M.Map T.Text T.Text -> Maybe Spellcasting
loadSpellcasting m = f cls abi bon sdc
  where
    cls = M.lookup "spellCastingClass0" m
    abi = M.lookup "spellCastingAbility0" m
    bon = M.lookup "spellAtkBonus0" m
    sdc = M.lookup "spellSaveDC0" m
    rs l = T.unpack $ M.findWithDefault "0" ("spellSlotHeader" <> l) m
    f (Just c) (Just a) (Just b) (Just d) = 
      Just Spellcasting { spellcastingClass = T.unpack c
                        , spellcastingAbility = T.unpack a
                        , spellSaveDC = T.unpack d
                        , spellAttackBonus = T.unpack b
                        , spells = mapMaybe (loadSpell m) [0..100]
                        , slotsLevel1 = rs "1"
                        , slotsLevel2 = rs "2"
                        , slotsLevel3 = rs "3"
                        , slotsLevel4 = rs "4"
                        , slotsLevel5 = rs "5"
                        , slotsLevel6 = rs "6"
                        , slotsLevel7 = rs "7"
                        , slotsLevel8 = rs "8"
                        , slotsLevel9 = rs "9"
                        }
    f _ _ _ _ = Nothing

loadSpell :: M.Map T.Text T.Text -> Int -> Maybe Spell
loadSpell m l = Spell 
  <$> ((== "O") <$> look "spellPrepared")
  <*> unlk "spellName"
  <*> (DataMaps.toSpellLevel <$> look "spellName")
  <*> unlk "spellCastingTime"
  <*> unlk "spellComponents"
  <*> unlk "spellDuration"
  <*> unlk "spellNotes"
  <*> unlk "spellPage"
  <*> unlk "spellRange"
  <*> unlk "spellSaveHit"
  <*> unlk "spellSource"
  where 
    l' = show l
    look n = M.lookup (T.pack (n <> l')) m
    unlk n = T.unpack <$> look n

toSpellLevel :: T.Text -> SpellLevel
toSpellLevel n = 
  case ms of
    Nothing -> Unknown
    (Just s) -> 
      case SD.spLevel s of
        0 -> Cantrip
        1 -> One
        2 -> Two
        3 -> Three
        4 -> Four
        5 -> Five
        6 -> Six
        7 -> Seven
        8 -> Eight
        9 -> Nine
        _ -> Unknown
  where
    ms = M.lookup n' SD.spellMap
    n' = if T.takeEnd 4 n `elem` [" <C>", " [R]"] then T.dropEnd 4 n else n

loadPdfField :: M.Map T.Text T.Text -> DataMap -> Character -> Character
loadPdfField m (f,s) c = case M.lookup f m of
                           Nothing -> c
                           Just v  -> s c v

dataMaps :: [DataMap]
dataMaps = 
  [ ("AC", (\c v -> c { acBase = readInt v }))
  , ("AGE", (\c v -> c { age = readInt v }))
  , ("ALIGNMENT", (\c v -> c { alignment = readAlign v }))
  -- , ("Acrobatics","+5")
  -- , ("AcrobaticsMod","DEX")
  , ("AcrobaticsProf", (\c v ->  c {skillAcrobatics = if v == "P" then 1.0 else 0.0 }))
  -- , ("AdditionalNotes1","")
  -- , ("AdditionalSenses","")
  -- , ("AlliesOrganizations","")
  -- , ("Animal","+3")
  , ("AnimalHandlingProf", (\c v ->  c {skillAnimalHandling = if v == "P" then 1.0 else 0.0 }))
  -- , ("AnimalMod","WIS")
  , ("Appearance", (\c v -> if v == "" then c else c { appearance = T.unpack v } ))
  -- , ("Arcana","+5")
  -- , ("ArcanaMod","INT")
  , ("ArcanaProf", (\c v ->  c {skillArcana = if v == "P" then 1.0 else 0.0 }))
  -- , ("Athletics","+2")
  -- , ("AthleticsMod","STR")
  , ("AthleticsProf", (\c v ->  c {skillAthletics = if v == "P" then 1.0 else 0.0 }))
  , ("BACKGROUND", (\c v -> c { background = T.unpack v } ))
  -- , ("BACKGROUND2","Urchin")
  , ("Backstory", (\c v -> c { backstory = lines (T.unpack v) } ))
  , ("Bonds", (\c v -> c { bonds = lines (T.unpack v) } ))
  , ("CHA",    readStat (\x c -> c { charisma = x } ))
  , ("CHamod", readStat (\x c -> c { charisma = x } ))
  , ("CLASS  LEVEL2", readLevel)
  -- , ("CLASS  LEVEL2","Wizard 5")
  , ("CON",    readStat (\x c -> c { constitution = x } ))
  , ("CONmod", readStat (\x c -> c { constitution = x } ))
  , ("CP", (\c v -> c { moneyPouch = (moneyPouch c) { copper = readInt v } } ))
  -- , ("ChaProf","")
  , ("CharacterName", (\c v -> c { characterName = T.unpack v } ))
  -- , ("CharacterName2","Gareth")
  -- , ("CharacterName4","Gareth")
  -- , ("ConProf","")
  , ("DEX",    readStat (\x c -> c { dexterity = x } ))
  , ("DEXmod", readStat (\x c -> c { dexterity = x } ))
  -- , ("Deception","+3")
  -- , ("DeceptionMod","CHA")
  , ("DeceptionProf", (\c v ->  c {skillDeception = if v == "P" then 1.0 else 0.0 }))
  -- , ("Defenses","")
  , ("EP", (\c v -> c { moneyPouch = (moneyPouch c) { electrum = readInt v } } ))
  , ("EXPERIENCE POINTS", (\c v -> c { experience = T.unpack v } ))
  -- , ("EXPERIENCE POINTS2","(Milestone)")
  , ("EYES", (\c v -> c { eyeColour = T.unpack v } ))
  -- , ("Encumbered","0 lb.")
  , ("FAITH", (\c v -> c { faith = T.unpack v } ))
  -- , ("FeaturesTraits3","")
  , ("Flaws", (\c v -> c { flaws = lines (T.unpack v) } ))
  , ("GENDER", (\c v -> c { gender = T.unpack v } ))
  , ("GP", (\c v -> c { moneyPouch = (moneyPouch c) { gold = readInt v } } ))
  , ("HAIR", (\c v -> c { hairColour = T.unpack v } ))
  , ("HEIGHT", (\c v -> c { height = T.unpack v } ))
  -- , ("History","+5")
  -- , ("HistoryMod","INT")
  , ("HistoryProf", (\c v ->  c {skillHistory = if v == "P" then 1.0 else 0.0 }))
  , ("INT",    readStat (\x c -> c { intelligence = x } ))
  , ("INTmod", readStat (\x c -> c { intelligence = x } ))
  -- , ("Init","+5")
  -- , ("Insight","+6")
  -- , ("InsightMod","WIS")
  , ("InsightProf", (\c v ->  c {skillInsight = if v == "P" then 1.0 else 0.0 }))
  -- , ("Intimidation","+3")
  -- , ("IntimidationMod","CHA")
  , ("IntimidationProf", (\c v ->  c {skillIntimidation = if v == "P" then 1.0 else 0.0 }))
  -- , ("Investigation","+8")
  -- , ("InvestigationMod","INT")
  , ("InvestigationProf", (\c v ->  c {skillInvestigation = if v == "P" then 1.0 else 0.0 }))
  , ("MaxHP", (\c v -> c { hitPoints = readInt v } ))
  -- , ("Medicine","+3")
  -- , ("MedicineMod","WIS")
  , ("MedicineProf", (\c v ->  c {skillMedicine = if v == "P" then 1.0 else 0.0 }))
  -- , ("Nature","+5")
  -- , ("NatureMod","INT")
  , ("NatureProf", (\c v ->  c {skillNature = if v == "P" then 1.0 else 0.0 }))
  , ("PLAYER NAME", (\c v -> c { playerName = T.unpack v }))
  -- , ("PLAYER NAME2","ScottLSedgwick")
  , ("PP", (\c v -> c { moneyPouch = (moneyPouch c) { platinum = readInt v } } ))
  -- , ("Passive1","13")
  -- , ("Passive2","16")
  -- , ("Passive3","18")
  -- , ("Perception","+3")
  -- , ("PerceptionMod","WIS")
  , ("PerceptionProf", (\c v ->  c {skillPerception = if v == "P" then 1.0 else 0.0 }))
  -- , ("Performance","+3")
  -- , ("PerformanceMod","CHA")
  , ("PerformanceProf", (\c v ->  c {skillPerformance = if v == "P" then 1.0 else 0.0 }))
  , ("PersonalityTraits", (\c v -> c { personalityTraits = lines (T.unpack v) } ))
  -- , ("Persuasion","+3")
  -- , ("PersuasionMod","CHA")
  , ("PersuasionProf", (\c v ->  c {skillPersuasion = if v == "P" then 1.0 else 0.0 }))
  -- , ("ProfBonus","+3")
  , ("ProficienciesLang", (\c v -> c { proficiencies = lines (T.unpack v) } ))
  -- , ("PushDragLift","0 lb.")
  , ("RACE", (\c v -> c { race = T.unpack v } ))
  -- , ("RACE2","Human")
  -- , ("Religion","+5")
  -- , ("ReligionMod","INT")
  , ("ReligionProf", (\c v ->  c {skillReligion = if v == "P" then 1.0 else 0.0 }))
  -- , ("SIZE","Medium")
  , ("SKIN", (\c v -> c { skinColour = T.unpack v } ))
  , ("SP", (\c v -> c { moneyPouch = (moneyPouch c) { silver = readInt v } } ))
  -- , ("ST Charisma","+3")
  -- , ("ST Constitution","+3")
  -- , ("ST Dexterity","+8")
  -- , ("ST Intelligence","+8")
  -- , ("ST Strength","+2")
  -- , ("ST Wisdom","+6")
  , ("STR",    readStat (\x c -> c { strength = x } ))
  , ("STRmod", readStat (\x c -> c { strength = x } ))
  -- , ("SaveModifiers","")
  , ("SleightOfHandProf", (\c v ->  c {skillSleightOfHand = if v == "P" then 1.0 else 0.0 }))
  -- , ("SleightofHand","+8")
  -- , ("SleightofHandMod","DEX")
  , ("Speed", (\c v -> c { speed = T.unpack v } ))
  -- , ("Stealth ","+8")
  -- , ("StealthMod","DEX")
  , ("StealthProf", (\c v ->  c {skillStealth = if v == "P" then 1.0 else 0.0 }))
  -- , ("StrProf","")
  -- , ("Survival","+3")
  -- , ("SurvivalMod","WIS")
  , ("SurvivalProf", (\c v ->  c {skillSurvival = if v == "P" then 1.0 else 0.0 }))
  -- , ("TempHP","--")
  , ("Total", (\c v -> c { hitDice = T.unpack v } ))
  , ("WEIGHT", (\c v -> c { weight = T.unpack v } ))
  , ("WIS",    readStat (\x c -> c { wisdom = x } ))
  , ("WISmod", readStat (\x c -> c { wisdom = x } ))
  -- , ("Weight Carried","0 lb.")
  ]

readInt :: T.Text -> Integer
readInt v = case (readMaybe (T.unpack v)) of
              Nothing -> (-1000)
              (Just x) -> x

readAlign :: T.Text -> Alignment
readAlign s | s == "Chaotic Good" = CG
            | s == "Neutral Good" = NG
            | s == "Lawful Good" = LG
            | s == "Chaotic Neutral" = CN
            | s == "Neutral" = N 
            | s == "Lawful Neutral" = LN
            | s == "Chaotic Evil" = CE
            | s == "Neutral Evil" = NE
            | s == "Lawful Evil" = LE
            | otherwise = None

readStat :: (Integer -> Character -> Character) -> Character -> T.Text -> Character
readStat f c v = if T.head v == '+' then c else f (readInt v) c

readLevel :: Character -> T.Text -> Character
readLevel c v = c { className = T.unpack cls, level = readInt lvl }
  where
    (cls, lvl) = T.breakOn " " v

emptyCharacter :: Character
emptyCharacter = Character
  { characterName = ""
  , sheetType = SheetTypeStandard
  , className = ""
  , subclass = ""
  , level = 0
  , background = ""
  , playerName = ""
  , race = ""
  , alignment = None
  , experience = ""
  , appearance = ""
  , gender = ""
  , faith = ""

  , strength = 0
  , dexterity = 0
  , constitution = 0
  , intelligence = 0
  , wisdom = 0
  , charisma = 0

  , strengthBonus = 0
  , dexterityBonus = 0
  , constitutionBonus = 0
  , intelligenceBonus = 0
  , wisdomBonus = 0
  , charismaBonus = 0

  , inspiration = 0
  , proficiencyBonus = 0

  , skillAcrobatics = 0.0
  , skillAnimalHandling = 0.0
  , skillArcana = 0.0
  , skillAthletics = 0.0
  , skillDeception = 0.0
  , skillHistory = 0.0
  , skillInsight = 0.0
  , skillIntimidation = 0.0
  , skillInvestigation = 0.0
  , skillMedicine = 0.0
  , skillNature = 0.0
  , skillPerception = 0.0
  , skillPerformance = 0.0
  , skillPersuasion = 0.0
  , skillReligion = 0.0
  , skillSleightOfHand = 0.0
  , skillStealth = 0.0
  , skillSurvival = 0.0

  , bonusAcrobatics = 0
  , bonusAnimalHandling = 0
  , bonusArcana = 0
  , bonusAthletics = 0
  , bonusDeception = 0
  , bonusHistory = 0
  , bonusInsight = 0
  , bonusIntimidation = 0
  , bonusInvestigation = 0
  , bonusMedicine = 0
  , bonusNature = 0
  , bonusPerception = 0
  , bonusPerformance = 0
  , bonusPersuasion = 0
  , bonusReligion = 0
  , bonusSleightOfHand = 0
  , bonusStealth = 0
  , bonusSurvival = 0
  
  , proficiencies = []

  , acBase = 0
  , acBonus = 0
  , initiative = 0
  , speed = ""
  , hitPoints = 0
  , tempHitPoints = 0
  , hitDice = ""
  
  , attacks = []

  , moneyPouch = Cash 
     { copper = 0
     , silver = 0
     , electrum = 0
     , gold = 0
     , platinum = 0
     , gems = []
     }
  , backpacks = []
  , bagsOfHolding = []
  , portableHoles = []
  , equipment = []

  , personalityTraits = []
  , ideals = []
  , bonds = []
  , flaws = []
  , features = []
  
  , age = 0
  , height = ""
  , weight = ""
  , eyeColour = ""
  , skinColour = ""
  , hairColour = ""
  , backstory = []
  , allies = []
  , treasure = []
  
  , spellcasting = Nothing
  }