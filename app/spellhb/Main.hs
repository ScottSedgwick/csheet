{-# LANGUAGE OverloadedStrings #-}
module Main where

import DataTypes
import qualified SpellData as SD

import Data.Aeson(eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Options.Applicative
import System.Exit (die)

data Options = Options
  { json :: String
  , outfile :: String
  }

options :: Parser Options
options = Options
  <$> strOption ( long "json"      <> short 'j' <> metavar "JSON" <> help "JSON file to read values from" <> showDefault <> value "characters/strahd/rusty/rusty.5.json" )
  <*> strOption ( long "outfile"   <> short 'o' <> metavar "OUT"  <> help "File to write to" <> showDefault <> value "characters/strahd/rusty/rusty.5.hb" )

main :: IO()
main = run =<< execParser opts
  where
    opts = info (options <**> helper) ( fullDesc <> progDesc "Read data from JSON file and generate spell book" <> header "spellhb - DND Spell Book Generator" )

run :: Options -> IO()
run opts = do
  chardata <- B.readFile (json opts)
  case parseCharacter chardata of
    Left err -> putStrLn "Error parsing JSON:" >> die err
    Right ch -> buildSheet (outfile opts) ch

parseCharacter :: B.ByteString -> Either String Character
parseCharacter = eitherDecode

levelNum :: Spell -> String
levelNum s = 
  case spellLevel s of
    Cantrip -> "0"
    One -> "1"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Unknown -> "X"

buildSheet :: String -> Character -> IO()
buildSheet outfilename ch = do
  case spellcasting ch of
    Nothing -> die "Error: Character has no spellcasting ability"
    Just sc -> do
        let xs = L.sortOn (\s -> levelNum s <> spellName s) (spells sc)
        T.writeFile outfilename $ book (characterName ch) xs
        putStrLn "Paste the output into Homebrewery (https://homebrewery.naturalcrit.com/)"
        putStrLn "Don't forget to put in page breaks"

book :: String -> [Spell] -> T.Text
book name xs = "# Spellbook for " <> T.pack name <> "\n\n"
  <> mconcat (map mkspell xs)

mkspell :: Spell -> T.Text
mkspell sp = case findSpell sp of
               Nothing -> "### No spell data found for " <> T.pack (spellName sp) <> "\n\n"
               Just s  -> renderspell s

findSpell :: Spell -> Maybe SD.Spell
findSpell sp = L.find (\s -> T.isPrefixOf (SD.spName s) x) SD.spells
  where
    x = T.pack (spellName sp)

--isPrefixOf :: Text -> Text -> Bool
    
renderspell :: SD.Spell -> T.Text
renderspell sp = "#### " <> SD.spName sp <> "\n\n"
  <> "*" <> mklevel (SD.spLevel sp) (SD.spType sp) <> mkritual (SD.spRitual sp) <> "*\n\n"
  <> "**Casting Time:** " <> SD.spTime sp <> "\n\n"
  <> "**Range:** " <> SD.spRange sp <> "\n\n"
  <> "**Components:** " <> SD.spComponents sp <> "\n\n"
  <> "**Duration:** " <> SD.spDuration sp <> "\n\n"
  <> SD.spDescription sp <> "\n\n"
  <> mkHigher (SD.spHigher sp)

mkHigher :: Maybe T.Text -> T.Text
mkHigher Nothing = ""
mkHigher (Just x) = "***At Higher Levels.*** " <> x <> "\n\n"

mklevel :: Int -> SD.SpellType -> T.Text
mklevel 0 t = mktype t <> " cantrip"
mklevel 1 t = "1st level " <> mktype t
mklevel 2 t = "2nd level " <> mktype t
mklevel 3 t = "3rd level " <> mktype t
mklevel 4 t = "4th level " <> mktype t
mklevel 5 t = "5th level " <> mktype t
mklevel 6 t = "6th level " <> mktype t
mklevel 7 t = "7th level " <> mktype t
mklevel 8 t = "8th level " <> mktype t
mklevel 9 t = "9th level " <> mktype t
mklevel _ _ = "WTF? Invalid spell level"

mktype :: SD.SpellType -> T.Text
mktype SD.Abjuration = "Abjuration"
mktype SD.Conjuration = "Conjuration"
mktype SD.Divination = "Divination"
mktype SD.Enchantment = "Enchantment"
mktype SD.Evocation = "Evocation"
mktype SD.Illusion = "Illusion"
mktype SD.Necromancy = "Necromancy"
mktype SD.Transmutation = "Transmutation"

mkritual :: Bool -> T.Text
mkritual False = ""
mkritual True  = " (ritual)"