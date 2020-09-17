module Utilities where

import DataTypes
import Graphics.PDF(PDFFont, PDFFloat, Draw, Complex(..), Circle(..), drawText, text, toPDFString, moveto, lineto, strokePath, setWidth, fill)

drawMyText :: PDFFont -> PDFFloat -> PDFFloat -> String -> Draw()
drawMyText fnt x y s = drawText $ text fnt x y (toPDFString s)

drawMyStrings :: PDFFont -> PDFFloat -> PDFFloat -> PDFFloat -> [String] -> Draw()
drawMyStrings _   _ _ _  []     = return ()
drawMyStrings fnt x y dy (s:ss) = do
  drawMyText fnt x y s
  drawMyStrings fnt x (y - dy) dy ss

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
drawMyAttack fnt x y (Attack w d b) = do
  drawMyText fnt x y w
  drawMyText fnt (x + 68) y b
  drawMyText fnt (x + 103) y d

drawGrid :: Draw()
drawGrid = do
  mapM_ (\x -> drawVLine (x * 10)) [0..59]
  mapM_ (\x -> drawHLine (x * 10)) [0..84]
  return ()

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

drawCircle :: PDFFloat -> PDFFloat -> PDFFloat -> Integer -> Draw()
drawCircle _ _ _ 0 = return ()
drawCircle x y r m = do
  setWidth 3
  fill (Circle x y (r - 2 + (intToPDFFloat m * 2)))


statBonus :: Integer -> Integer
statBonus stat = if sb > 10 then 10 else sb
  where
    sb = (stat - 10) `div` 2

saveBonus :: Integer -> Integer -> Integer -> Integer
saveBonus stat saveB prof = statBonus stat + saveB * prof

profBonus :: Character -> Integer
profBonus c = (level c - 1) `div` 4 + 2 + proficiencyBonus c

skillBonus :: Integer -> Integer -> Integer -> Integer -> Integer
skillBonus stat saveB prof profB = (statBonus stat) + (prof * profB) -- + (profB * saveB)

showClass :: Character -> String
showClass c = if lc == 0 then "" else cc ++ ssc ++ slc
  where 
    cc = className c
    sc = subclass c
    lc = level c
    ssc = if sc == "" then "" else " (" ++ sc ++ ")"
    slc = " [" ++ (show lc) ++ "]"