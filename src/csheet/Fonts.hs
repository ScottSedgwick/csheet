module Fonts where

import Graphics.PDF (AnyFont(..), PDFFont(..), FontName(..), FontSize, StdFont(..), mkStdFont)

data Fonts = Fonts
  { fontLarge :: PDFFont 
  , fontNormal :: PDFFont
  , fontSmall :: PDFFont
  , fontTiny :: PDFFont
  , fontTeeny :: PDFFont
  , helvetica :: AnyFont
  }

loadFonts :: IO Fonts
loadFonts = do
  Just fLarge <- fntLarge
  Just fNormal <- fntNormal
  Just fSmall <- fntSmall
  Just fTiny <- fntTiny
  Just fTeeny <- fntTeeny
  Right fHelvetica <- mkStdFont Helvetica
  return Fonts
           { fontLarge = fLarge
           , fontNormal = fNormal
           , fontSmall = fSmall
           , fontTiny = fTiny
           , fontTeeny = fTeeny
           , helvetica = fHelvetica
           }

mkFont :: FontName -> FontSize -> IO (Maybe PDFFont)
mkFont fn fs = do
  Right font <- mkStdFont fn
  return $ Just (font `PDFFont` fs)

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

-- embeddedPalatinoR :: ByteString
-- embeddedPalatinoR = $(embedFile "fonts/palatino_regular.afm")

-- mkPalatinoR :: IO (Maybe AnyFont)
-- mkPalatinoR = do
--   theEncoding <- getEncoding AdobeStandardEncoding
--   theMacEncoding <- parseMacEncoding 
--   maybeFs <- getFont (Left $ unpack embeddedPalatinoR) theEncoding (Just theMacEncoding)
--   case maybeFs of 
--     Just theFont -> do
--       let f' = theFont { baseFont = show embeddedPalatinoR
--                        }
--       return . Just . AnyFont . StdFont $ f'
--     Nothing -> return Nothing

-- fntSpellTitle :: IO (Maybe PDFFont)
-- fntSpellTitle = mkFont Copperplate_Light 12