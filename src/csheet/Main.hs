module Main where

import DataTypes
import Fonts (Fonts, loadFonts)
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
  { page1 :: Character -> JpegFile -> Fonts -> PDF()
  , page2 :: Character -> JpegFile -> Fonts -> PDF()
  , pageSpells :: JpegFile -> Fonts -> Spellcasting -> PDF()
  , pageBags :: JpegFile -> Character -> Fonts -> Backpack -> PDF()
  , pageBoh :: JpegFile -> Character -> Fonts -> BagOfHolding -> PDF()
  , pageHole :: JpegFile -> Character -> Fonts -> PortableHole -> PDF() 
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
  fonts <- loadFonts
  B.writeFile (charname ++ ".pdf") $ pdfByteString standardDocInfo (PDFRect 0 0 595 842) (doc ch images pages fonts)

doc :: Character -> Images -> Pages -> Fonts -> PDF()
doc c images pages fonts = do
  page1 pages c (imgPage1 images) fonts
  page2 pages c (imgPage2 images) fonts
  mapM_ (pageSpells pages (imgSpells images) fonts) (spellcasting c)
  mapM_ (pageBags pages (imgBags images) c fonts) (backpacks c)
  mapM_ (pageBoh pages (imgBoh images) c fonts) (bagsOfHolding c)
  mapM_ (pageHole pages (imgHole images) c fonts) (portableHoles c)
  -- drawAbilities c (abilities c) fonts
