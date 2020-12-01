module Fonts where

import Data.Maybe (fromJust)
import Graphics.PDF (PDFFont(..), FontName(..), FontSize, mkStdFont)

data Fonts = Fonts
  { fontLarge :: PDFFont 
  , fontNormal :: PDFFont
  , fontSmall :: PDFFont
  , fontTiny :: PDFFont
  , fontTeeny :: PDFFont
  }

loadFonts :: IO Fonts
loadFonts = do
  fLarge <- fntLarge
  fNormal <- fntNormal
  fSmall <- fntSmall
  fTiny <- fntTiny
  fTeeny <- fntTeeny
  return Fonts
           { fontLarge = fromJust fLarge
           , fontNormal = fromJust fNormal
           , fontSmall = fromJust fSmall
           , fontTiny = fromJust fTiny
           , fontTeeny = fromJust fTeeny
           }

mkFont :: FontName -> FontSize -> IO (Maybe PDFFont)
mkFont fn fs = do
  font <- mkStdFont fn
  return $ (`PDFFont` fs) <$> font

fntLarge :: IO (Maybe PDFFont)
fntLarge = mkFont Helvetica_Bold 14

fntNormal :: IO (Maybe PDFFont)
fntNormal = mkFont Helvetica 12

fntSmall :: IO (Maybe PDFFont)
fntSmall = mkFont Helvetica 10

fntTiny :: IO (Maybe PDFFont)
fntTiny = mkFont Helvetica 8

fntTeeny :: IO (Maybe PDFFont)
fntTeeny = mkFont Helvetica 7