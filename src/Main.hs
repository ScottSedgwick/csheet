module Main where

import DataTypes
import Pages

import Data.Aeson(eitherDecode)
import Data.Either(rights)
import Graphics.PDF(JpegFile, PDF, readJpegFile, pdfByteString, standardDocInfo, PDFRect(..))
import System.Environment (getArgs)
import System.Exit (die)
import qualified Data.ByteString.Lazy as B

parseCharacter :: B.ByteString -> Either String Character
parseCharacter = eitherDecode

defaultCharName :: String
defaultCharName = "characters/Sharwyn_06"

getCharName :: IO String
getCharName = do
  xs <- getArgs
  return $ if length xs == 0 then defaultCharName else head xs

data Images = Images 
  { page1 :: JpegFile
  , page2 :: JpegFile
  , spells :: JpegFile
  , bags :: JpegFile
  , boh :: JpegFile
  , hole :: JpegFile
  }

main :: IO ()
main = do
  charname   <- getCharName
  mbg1       <- readJpegFile "images/page1.jpg"
  mice1      <- readJpegFile "images/icewinddalePage1.jpg"
  mbg2       <- readJpegFile "images/page2.jpg"
  mice2      <- readJpegFile "images/icewinddalePage2.jpg"
  mbgSpells  <- readJpegFile "images/pageSpells.jpg"
  miceSpells <- readJpegFile "images/icewinddaleSpells.jpg"
  mbgBags    <- readJpegFile "images/pageBag.jpg"
  mbgBOH     <- readJpegFile "images/bag-of-holding.jpg"
  mbgPH      <- readJpegFile "images/portable-hole.jpg"
  let [bg1, bg2, bgSpells, bgBags, bgBOH, bgPH, ice1, ice2, iceSpells] = rights [mbg1, mbg2, mbgSpells, mbgBags, mbgBOH, mbgPH, mice1, mice2, miceSpells]
  let images = Images 
        { page1 = bg1
        , page2 = bg2
        , spells = bgSpells
        , bags = bgBags
        , boh = bgBOH
        , hole = bgPH
        }
  let charFile = charname ++ ".json"
  chardata <- B.readFile charFile
  case parseCharacter chardata of
    Left err -> do
      putStrLn "Error parsing JSON:"
      die err
    Right ch ->  B.writeFile (charname ++ ".pdf") $ pdfByteString standardDocInfo (PDFRect 0 0 595 842) (doc ch images)


doc :: Character -> Images -> PDF()
doc c images = do
  drawPage1 c (page1 images)
  drawPage2 c (page2 images)
  mapM_ (drawSpellPage $ spells images) (spellcasting c)
  mapM_ (drawBackpackPage (bags images) c) (backpacks c)
  mapM_ (drawBagOfHolding (boh images) c) (bagsOfHolding c)
  mapM_ (drawPortableHole (hole images) c) (portableHoles c)
  drawAbilities c (abilities c)


