module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Pdf.FieldReader (readPdfFields)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import Options.Applicative

import DataTypes
import DataMaps

data Options = Options
  { pdffile :: String
  , debug :: Bool
  }

options :: Parser Options
options = Options
  <$> strOption ( long "pdf"  <> short 'p' <> metavar "PDFFILE"  <> help "PDF file to read values from" <> showDefault <> value "characters/strahd/gareth/gareth.dndb.-p 5.pdf" )
  <*> switch ( long "debug"   <> short 'd' <> help "Output debug information" )

main :: IO()
main = run =<< execParser opts
  where
    opts = info (options <**> helper) ( fullDesc <> progDesc "Read values from PDF and write them to JSON" <> header "pdf - a DND Beyond PDF reader" )
     
run :: Options -> IO()
run opts = do
  debugPrint "Loading PDF file:" (pdffile opts)
  xs <- B.readFile (pdffile opts)
  debugPrint "Parsing PDF file" ()
  let ys = trimFldNames $ readPdfFields xs
  debugPrint "Fields from PDF:" ys
  let zs = loadPdfData ys
  debugPrint "Parsed data:" zs
  debugPrint "Unparsed data:" (listUnparsedFields ys)
  let js = A.encode zs
  putStrLn (BL.unpack js)
  where
    debugPrint c x = 
      if debug opts then do
        putStrLn c
        print x
      else pure ()

trimFldNames :: M.Map TS.Text TS.Text -> M.Map T.Text T.Text
trimFldNames = M.foldrWithKey f M.empty
  where
    f k a b = M.insert (g (TS.dropAround isSpace k)) (g a) b
    g = T.pack . TS.unpack
