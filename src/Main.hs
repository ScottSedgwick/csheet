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
  return $ if null xs then defaultCharName else head xs

data Images = Images 
  { imgPage1 :: JpegFile
  , imgPage2 :: JpegFile
  , imgSpells :: JpegFile
  , imgBags :: JpegFile
  , imgBoh :: JpegFile
  , imgHole :: JpegFile
  }

data Pages = Pages
  { page1 :: Character -> JpegFile -> PDF()
  , page2 :: Character -> JpegFile -> PDF()
  , pageSpells :: JpegFile -> Spellcasting -> PDF()
  , pageBags :: JpegFile -> Character -> Backpack -> PDF()
  , pageBoh :: JpegFile -> Character -> BagOfHolding -> PDF()
  , pageHole :: JpegFile -> Character -> PortableHole -> PDF() 
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
        { imgPage1 = bg1
        , imgPage2 = bg2
        , imgSpells = bgSpells
        , imgBags = bgBags
        , imgBoh = bgBOH
        , imgHole = bgPH
        }
  let pages = Pages
        { page1 = drawPage1
        , page2 = drawPage2
        , pageSpells = drawSpellPage
        , pageBags = drawBackpackPage
        , pageBoh = drawBagOfHolding
        , pageHole = drawPortableHole
        }
  let charFile = charname ++ ".json"
  chardata <- B.readFile charFile
  case parseCharacter chardata of
    Left err -> do
      putStrLn "Error parsing JSON:"
      die err
    Right ch ->  B.writeFile (charname ++ ".pdf") $ pdfByteString standardDocInfo (PDFRect 0 0 595 842) (doc ch images pages)

doc :: Character -> Images -> Pages -> PDF()
doc c images pages = do
  page1 pages c (imgPage1 images)
  page2 pages c (imgPage2 images)
  mapM_ (pageSpells pages $ imgSpells images) (spellcasting c)
  mapM_ (pageBags pages (imgBags images) c) (backpacks c)
  mapM_ (pageBoh pages (imgBoh images) c) (bagsOfHolding c)
  mapM_ (pageHole pages (imgHole images) c) (portableHoles c)
  drawAbilities c (abilities c)
