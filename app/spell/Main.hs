{-# LANGUAGE OverloadedStrings #-}
module Main where

import SpellData

import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T -- (Text, lines, pack)
import qualified Data.Text.Lazy.IO as T -- (readFile, writeFile)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, toLazyText)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

main :: IO()
main = do
  efn <- getFilename
  case efn of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 1)
    Right fn -> do
      str <- T.readFile fn
      let xs = T.lines str
      let ys = map lookupSpell xs
      saveSpellBook (fn <> ".tex") ys

lookupSpell :: T.Text -> Either T.Text Spell
lookupSpell s =
  case M.lookup s spellMap of
    Nothing -> Left $ "No spell called [" <> s <> "]"
    (Just s) -> Right s

getFilename :: IO (Either String String)
getFilename = do
  xs <- getArgs
  if null xs 
    then return (Left "No filename argument found") 
    else do
      let fn = head xs ++ ".json"
      b <- doesFileExist fn
      if b
        then return (Right fn)
        else return (Left ("File [" ++ fn ++ "] not found."))

saveSpellBook :: FilePath -> [Either T.Text Spell] -> IO()
saveSpellBook fn xs = T.writeFile fn (toLazyText $ buildSpellBook xs)

buildSpellBook :: [Either T.Text Spell] -> Builder
buildSpellBook xs 
  =  fromText "\\documentclass[10pt, a4paper, twocolumn]{book}" 
  <> fromText "\\title{Spell Book}" 
  <> fromText "\\date{2020-08-27}" 
  <> fromText "\\author{Scott Sedgwick}" 
  <> fromText "\\usepackage[dvipsnames]{xcolor}" 
  <> fromText "\\definecolor{parchment}{RGB}{255,255,250}" 
  <> fromText "" 
  <> fromText "\\newcommand{\\spell}[7]{\\textcolor{Maroon}{\\large\\textsc{#1}}\\\\" 
  <> fromText "\\normalsize\\textit{#2}\\\\" 
  <> fromText "\\textbf{Casting Time:} #3\\\\" 
  <> fromText "\\textbf{Range:} #4\\\\" 
  <> fromText "\\textbf{Components:} #5\\\\" 
  <> fromText "\\textbf{Duration:} #6\\\\" 
  <> fromText "#7\\\\}" 
  <> fromText "" 
  <> fromText "\\newcommand{\\sphigh}[1]{\\textbf{At Higher Levels.} #1\\\\}" 
  <> fromText "" 
  <> fromText "\\begin{document}" 
  <> fromText "\\pagecolor{parchment}" 
  <> bintercalate (lf <> pb <> lf) (map (buildLevel xs) [0..9])
  <> outputMissingSpells xs
  <> fromText "\\end{document}"

buildLevel :: [Either T.Text Spell] -> Int -> Builder
buildLevel xs lvl = do
  let spells = filterSpells lvl xs
  if null spells
    then mempty
    else heading (lvlCaption lvl) <> bintercalate lf (map buildSpell spells)

bintercalate :: Builder -> [Builder] -> Builder
bintercalate sep xs = f (filter (/= mempty) xs)
  where
    f []     = mempty
    f [x]    = x
    f (x:xs) = (if x == mempty then mempty else x <> sep) <> bintercalate sep xs

lf :: Builder
lf = fromText "\\\\"

pb :: Builder
pb = fromText "\\pagebreak"

heading :: Builder -> Builder
heading s = fromText "\\Huge\\textsc{" <> s <> fromText "}" <> lf

filterSpells :: Int -> [Either T.Text Spell] -> [Spell]
filterSpells lvl = foldr f []
  where
    f (Left err) xs = xs
    f (Right s) xs = if spLevel s == lvl then s:xs else xs

buildSpell :: Spell -> Builder
buildSpell sp 
  =  fromText "\\spell{" 
  <> fromLazyText (spName sp) <> fromText "}{" 
  <> spellType (spLevel sp) (spType sp) <> ritual (spRitual sp) <> fromText "}{" 
  <> fromLazyText (spTime sp) <> fromText "}{" 
  <> fromLazyText (spRange sp) <> fromText "}{" 
  <> fromLazyText (spComponents sp) <> fromText "}{" 
  <> fromLazyText (spDuration sp) <> fromText "}{" 
  <> fromLazyText (spDescription sp) <> fromText "}" 
  <> case spHigher sp of
      Nothing  -> mempty
      (Just h) -> fromText "\\sphigh{" <> fromLazyText h <> fromText "}"

spellType :: Int -> SpellType -> Builder
spellType 0 t = fromLazyText (T.pack (show t ++ " Cantrip"))
spellType n t = lvlCaption n <> fromText " " <> fromLazyText (T.pack (show t))

lvlCaption :: Int -> Builder
lvlCaption 0 = fromText "Cantrip"
lvlCaption 1 = fromText "1st Level"
lvlCaption 2 = fromText "2nd Level"
lvlCaption 3 = fromText "3rd Level"
lvlCaption 4 = fromText "4th Level"
lvlCaption 5 = fromText "5th Level"
lvlCaption 6 = fromText "6th Level"
lvlCaption 7 = fromText "7th Level"
lvlCaption 8 = fromText "8th Level"
lvlCaption 9 = fromText "9th Level"
lvlCaption _ = fromText "What?"

ritual :: Bool -> Builder
ritual False = mempty
ritual True = fromText " (ritual)"

outputMissingSpells :: [Either T.Text Spell] -> Builder
outputMissingSpells [] = mempty
outputMissingSpells ((Left err):xs) = lf <> fromLazyText err <> outputMissingSpells xs
outputMissingSpells ((Right _):xs) = outputMissingSpells xs

-- # Spellbook for Rusty

-- #### Locate Creature
-- *4th-level divination*

-- **Casting Time:** 1 action

-- **Range:**: Self

-- **Components:** V, S, M (a bit of fur from a bloodhound)

-- **Duration:** Concentration, up to 1 hour

-- Describe of name a creature that is familiar to you. You sense the direction of the creature's location, as long as that creature is within 1,000 feet of you. If the creature is moving, you know the direction of movement.

-- The spell can locate a specific creature known to you, or the nearest creature of a specific kind (such as a human or unicorn), so long as you have seen such a creature up close - within 30 feet - at least once. If the creature you described or named isin a different form, such as being under the effects of a *polymorph* spell, this spell doesn't locate that creature.

-- This spell can't locate a creature if running water at least 10 feet wide blocks a direct path between you and the creature.

-- #### Locate Object
