{-# LANGUAGE MultiParamTypeClasses #-}
module Utilities where

import DataTypes
import Data.Maybe   (mapMaybe)
import Data.List    (sortBy, sortOn)
import Data.Text    (pack)
import qualified Data.Map as M
import Graphics.PDF
import Graphics.PDF.Shapes
import Graphics.PDF.Typesetting

import Fonts(Fonts(..))
import Spells (spellMap)

drawMyText :: PDFFont -> PDFFloat -> PDFFloat -> String -> Draw()
drawMyText fnt x y s = drawText $ text fnt x y (pack s)

drawRaText :: PDFFont -> PDFFloat -> PDFFloat -> String -> Draw()
drawRaText fnt x y s = drawText $ text fnt x' y (pack s)
  where
    x' = x - textWidth fnt (pack s)

drawCntText :: PDFFont -> PDFFloat -> PDFFloat -> String -> Draw()
drawCntText fnt x y s = drawText $ text fnt x' y (pack s)
  where
    x' = x - (textWidth fnt (pack s) / 2)

drawMyStrings :: PDFFont -> PDFFloat -> PDFFloat -> PDFFloat -> [String] -> Draw()
drawMyStrings _   _ _ _  []     = return ()
drawMyStrings fnt x y dy (s:ss) = do
  drawMyText fnt x y s
  drawMyStrings fnt x (y - dy) dy ss

drawMySpells :: PDFFont -> PDFFloat -> PDFFloat -> PDFFloat -> [KnownSpell] -> Draw()
drawMySpells fnt x y dy ss = dms fnt x y dy ss
  where
    dms _   _ _ _  []     = return ()
    dms fnt x y dy (s:ss) = do
      if prepared s then drawCircle (x - 15.5) (y + 1.2) 3.1 1 else pure()
      drawMyText fnt (x - 9) y (spellName s)
      drawMySpells fnt x (y - dy) dy ss

data MyVertStyles = NormalPara
newtype MyParaStyles = Normal AnyFont
instance ComparableStyle MyParaStyles where
  isSameStyleAs (Normal fa) (Normal fb) = fa == fb
instance Style MyParaStyles where
    textStyle (Normal f) = TextStyle (PDFFont f 9) black black FillText 1.0 1.0 2.0 1.0
instance ParagraphStyle MyVertStyles MyParaStyles where
    lineWidth _ w _ = w
instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True

drawTxtBox :: AnyFont -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> String -> Draw()
drawTxtBox fnt x y w h s = do
  let p1 = x :+ y
  let p2 = (x + w) :+ (y + h)
  displayFormattedText (Rectangle p1 p2) NormalPara (Normal fnt) $ do
    setJustification LeftJustification
    paragraph $ txt $ pack s

drawMyItems :: PDFFont -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> [BagItem] -> Draw()
drawMyItems _   _  _  _  _ _  []     = return ()
drawMyItems fnt x1 x2 x3 y dy (x:xs) = do
  drawMyText fnt x1 y (biName x)
  drawMyText fnt x2 y (show $ biQty x)
  drawMyText fnt x3 y (show $ biLbs x)
  drawMyItems fnt x1 x2 x3 (y - dy) dy xs

drawMyAttacks :: PDFFont -> PDFFloat -> PDFFloat -> PDFFloat -> [Attack] -> Draw()
drawMyAttacks _   _ _ _  []     = return ()
drawMyAttacks fnt x y dy (s:ss) = do
  drawMyAttack fnt x y s
  drawMyAttacks fnt x (y - dy) dy ss

drawMyAttack :: PDFFont -> PDFFloat -> PDFFloat -> Attack -> Draw()
drawMyAttack fnt x y (Attack w d b _) = do
  drawMyText fnt x y w
  drawMyText fnt (x + 68) y b
  drawMyText fnt (x + 103) y d

drawGrid :: Draw()
drawGrid = do
  mapM_ (\x -> drawVLine (x * 10)) [0..59]
  mapM_ (\x -> drawHLine (x * 10)) [0..84]

drawVLine :: PDFFloat -> Draw()
drawVLine x = do
  moveto (x :+ 0)
  lineto (x :+ 842)
  strokePath

drawHLine :: PDFFloat -> Draw()
drawHLine y = do
  moveto (30 :+ y)
  lineto (565 :+ y)
  strokePath

intToPDFFloat :: Integer -> PDFFloat
intToPDFFloat = fromIntegral

drawCircle :: PDFFloat -> PDFFloat -> PDFFloat -> Double -> Draw()
drawCircle _ _ _ 0 = return ()
drawCircle x y r m =
  if m < 1
    then return ()
    else do
      setWidth 3
      fill (Circle x y (r - 2 + (m * 2)))

drawHollowCircle :: PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Draw()
drawHollowCircle x y r w = do
  setWidth w
  stroke $ Circle x y r

drawPlus :: PDFFloat -> PDFFloat -> PDFFloat -> Draw()
drawPlus x y l = do
  setWidth 0.5
  stroke $ Line (x - l) y (x + l) y
  stroke $ Line x (y - l) x (y + l)

drawAdvantage :: Fonts -> PDFFloat -> PDFFloat -> PDFFloat -> Double -> Double -> Draw()
drawAdvantage fs x y r ad sk =
  if (ad == 0) then pure()
  else if (ad == 1)
  then if (sk > 0)
       then do
         drawPlus x y r
         drawHollowCircle x y r 0.5
       else drawPlus x y (r - 0.2)
  else do
    drawMyText (fontTeeny fs) (x + 11) (y - 1) ("+d" <> show (fromIntegral $ truncate ad))

statBonus :: Integer -> Integer
statBonus stat = if sb > 10 then 10 else sb
  where
    sb = (stat - 10) `div` 2

saveBonus :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
saveBonus stat saveP saveB prof additional = statBonus stat + saveP * (prof + additional) + saveB

profBonus :: Character -> Integer
profBonus c = (level c - 1) `div` 4 + 2 + proficiencyBonus c

skillBonus :: Integer -> Double -> Integer -> Integer -> Integer
skillBonus stat skillMultiple profB skillAdvantage = statBonus stat + round (skillMultiple * fromIntegral profB) + (skillAdvantage * 5)

showClass :: Character -> String
showClass c = if lc == 0 then "" else cc ++ slc
  where
    cc = className c
    lc = level c
    slc = " [" ++ (show lc) ++ "]"

showSubClass :: Character -> String
showSubClass = subclass

spellsBy :: SpellLevel -> [KnownSpell] -> [KnownSpell]
spellsBy lvl = sortBy spellComparator . filter (matchSpellLevel lvl)

matchSpellLevel :: SpellLevel -> KnownSpell -> Bool
matchSpellLevel lvl s = (spLevel <$> M.lookup (spellName s) spellMap) == Just lvl

spellComparator :: KnownSpell -> KnownSpell -> Ordering
spellComparator a b =
  case compare (prepared a) (prepared b) of
    LT -> GT
    GT -> LT
    EQ -> compare (spellName a) (spellName b)
