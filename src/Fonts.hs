module Fonts where

import Graphics.PDF(PDFFont(..), FontName(..))

fntLarge :: PDFFont
fntLarge = PDFFont Helvetica_Bold 14

fntSmall :: PDFFont
fntSmall = PDFFont Helvetica 10

fntTiny :: PDFFont
fntTiny = PDFFont Helvetica 8

fntTeeny :: PDFFont
fntTeeny = PDFFont Helvetica 7