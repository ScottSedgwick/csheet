module Main where

import DataTypes
import Fonts (Fonts, loadFonts)
import Pages

import Data.Aeson(eitherDecode)
import Data.Either(rights)
import Graphics.PDF(JpegFile, PDF, readJpegFile, pdfByteString, standardDocInfo, PDFRect(..))
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)
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
  , imgSpellbook :: JpegFile
  , imgBags :: JpegFile
  , imgBoh :: JpegFile
  , imgHole :: JpegFile
  , imgAppearance :: Maybe JpegFile
  }

data Pages = Pages
  { page1 :: Character -> JpegFile -> Fonts -> PDF()
  , page2 :: Character -> JpegFile -> Maybe JpegFile -> Fonts -> PDF()
  , pageSpells :: JpegFile -> Fonts -> Spellcasting -> PDF()
  , pageSpellbook :: JpegFile -> Fonts -> Spellcasting -> PDF()
  , pageBags :: JpegFile -> Character -> Fonts -> Backpack -> PDF()
  , pageBoh :: JpegFile -> Character -> Fonts -> BagOfHolding -> PDF()
  , pageHole :: JpegFile -> Character -> Fonts -> PortableHole -> PDF() 
  }

data Options = Options
  { json :: String
  , pdf :: String
  , template :: String
  }

options :: Parser Options
options = Options
  <$> strOption ( long "json"      <> short 'j' <> metavar "JSON" <> help "JSON file to read values from" <> showDefault <> value "characters/strahd/gareth/gareth.5.json" )
  <*> strOption ( long "pdf"       <> short 'p' <> metavar "PDF"  <> help "PDF file to write to" <> showDefault <> value "characters/strahd/gareth/gareth.5.pdf" )
  <*> strOption ( long "template"  <> short 't' <> metavar "TMPL" <> help "Sheet template to use (Standard or IcewindDale)" <> showDefault <> value "IcewindDale" )

main :: IO()
main = run =<< execParser opts
  where
    opts = info (options <**> helper) ( fullDesc <> progDesc "Read data from JSON file and generate character sheet" <> header "csheet - DND Character Sheet Generator" )

run :: Options -> IO()
run opts = do
  chardata <- B.readFile (json opts)
  case (toSheetType $ "SheetType" <> template opts) of
    Nothing -> putStrLn ("Sheet type invalid: " <> template opts)
    Just t  -> do
      case parseCharacter chardata of
        Left err -> putStrLn "Error parsing JSON:" >> die err
        Right ch -> buildSheet (pdf opts) (ch { sheetType = t })

standardImageFilenames :: [FilePath]
standardImageFilenames = ["images/page1.jpg", "images/page2.jpg", "images/pageSpells.jpg", "images/pageBag.jpg", "images/bag-of-holding.jpg", "images/portable-hole.jpg", "images/spellbook.jpg"]

icewindDaleImageFilenames :: [FilePath]
icewindDaleImageFilenames = ["images/icewinddalePage1.jpg", "images/icewinddalePage2.jpg", "images/icewinddaleSpells.jpg", "images/pageBag.jpg", "images/bag-of-holding.jpg", "images/portable-hole.jpg", "images/spellbook.jpg"]

loadImages :: FilePath -> Character -> IO Images
loadImages folder ch = 
  case sheetType ch of
    SheetTypeStandard -> loadImageFiles folder a standardImageFilenames
    SheetTypeIcewindDale -> loadImageFiles folder a icewindDaleImageFilenames
  where
    a = if appearance ch == "" then Nothing else Just (appearance ch)

loadImageFiles :: FilePath -> Maybe FilePath -> [FilePath] -> IO Images
loadImageFiles folder appfile filenames = do
  efs <- mapM readJpegFile filenames
  let [bg1, bg2, bgSpells, bgBags, bgBOH, bgPH, bgSpellbook] = rights efs
  app <- loadAppFile folder appfile
  return Images 
        { imgPage1 = bg1
        , imgPage2 = bg2
        , imgSpells = bgSpells
        , imgSpellbook = bgSpellbook
        , imgBags = bgBags
        , imgBoh = bgBOH
        , imgHole = bgPH
        , imgAppearance = app
        }

loadAppFile :: FilePath -> Maybe FilePath -> IO (Maybe JpegFile)
loadAppFile folder mfilename = maybe (return Nothing) (\f -> readJpegFile (folder </> f) >>= \eappj -> return $ either (const Nothing) Just eappj) mfilename

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
    , pageSpellbook = drawSpellbook
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
    , pageSpellbook = drawSpellbook
    , pageBags = drawBackpackPage
    , pageBoh = drawBagOfHolding
    , pageHole = drawPortableHole
    }

buildSheet :: String -> Character -> IO()
buildSheet outfilename ch = do
  let folder = takeDirectory outfilename
  images <- loadImages folder ch
  let pages = loadPages ch
  fonts <- loadFonts
  B.writeFile outfilename $ pdfByteString standardDocInfo (PDFRect 0 0 595 842) (doc ch images pages fonts)

doc :: Character -> Images -> Pages -> Fonts -> PDF()
doc c images pages fonts = do
  page1 pages c (imgPage1 images) fonts
  page2 pages c (imgPage2 images) (imgAppearance images) fonts
  mapM_ (pageBags pages (imgBags images) c fonts) (backpacks c)
  mapM_ (pageBoh pages (imgBoh images) c fonts) (bagsOfHolding c)
  mapM_ (pageHole pages (imgHole images) c fonts) (portableHoles c)
  mapM_ (pageSpells pages (imgSpells images) fonts) (spellcasting c)
  -- mapM_ (pageSpellbook pages (imgSpellbook images) fonts) (spellcasting c)
  -- drawAbilities c (abilities c) fonts
