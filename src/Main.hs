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
  charname <- getCharName
  chardata <- B.readFile (charname ++ ".json")
  case parseCharacter chardata of
    Left err -> putStrLn "Error parsing JSON:" >> die err
    Right ch -> buildSheet charname ch

standardImageFilenames :: [FilePath]
standardImageFilenames = ["images/page1.jpg", "images/page2.jpg", "images/pageSpells.jpg", "images/pageBag.jpg", "images/bag-of-holding.jpg", "images/portable-hole.jpg"]

icewindDaleImageFilenames :: [FilePath]
icewindDaleImageFilenames = ["images/icewinddalePage1.jpg", "images/icewinddalePage2.jpg", "images/icewinddaleSpells.jpg", "images/pageBag.jpg", "images/bag-of-holding.jpg", "images/portable-hole.jpg"]

loadImages :: Character -> IO Images
loadImages ch = 
  case sheetType ch of
    SheetTypeStandard -> loadImageFiles standardImageFilenames
    SheetTypeIcewindDale -> loadImageFiles icewindDaleImageFilenames

loadImageFiles :: [FilePath] -> IO Images
loadImageFiles filenames = do
  efs <- mapM readJpegFile filenames
  let [bg1, bg2, bgSpells, bgBags, bgBOH, bgPH] = rights efs
  return Images 
        { imgPage1 = bg1
        , imgPage2 = bg2
        , imgSpells = bgSpells
        , imgBags = bgBags
        , imgBoh = bgBOH
        , imgHole = bgPH
        }

loadPages :: Character -> Pages
loadPages ch =
  case sheetType ch of
    SheetTypeStandard -> loadStandardPages
    SheetTypeIcewindDale -> loadIcewindDalePages

loadStandardPages :: Pages
loadStandardPages = 
  Pages
    { page1 = drawPage1
    , page2 = drawPage2
    , pageSpells = drawSpellPage
    , pageBags = drawBackpackPage
    , pageBoh = drawBagOfHolding
    , pageHole = drawPortableHole
    }

loadIcewindDalePages :: Pages
loadIcewindDalePages = 
  Pages
    { page1 = drawIcePage1
    , page2 = drawIcePage2
    , pageSpells = drawIceSpellPage
    , pageBags = drawBackpackPage
    , pageBoh = drawBagOfHolding
    , pageHole = drawPortableHole
    }

buildSheet :: String -> Character -> IO()
buildSheet charname ch = do
  images <- loadImages ch
  let pages = loadPages ch
  B.writeFile (charname ++ ".pdf") $ pdfByteString standardDocInfo (PDFRect 0 0 595 842) (doc ch images pages)

doc :: Character -> Images -> Pages -> PDF()
doc c images pages = do
  page1 pages c (imgPage1 images)
  page2 pages c (imgPage2 images)
  mapM_ (pageSpells pages $ imgSpells images) (spellcasting c)
  mapM_ (pageBags pages (imgBags images) c) (backpacks c)
  mapM_ (pageBoh pages (imgBoh images) c) (bagsOfHolding c)
  mapM_ (pageHole pages (imgHole images) c) (portableHoles c)
  drawAbilities c (abilities c)
