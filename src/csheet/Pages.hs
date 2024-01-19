{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Pages where

import DataTypes 
import Fonts(Fonts(..))
import Spells
import Utilities(drawCntText, drawTxtBox, drawMyText, drawCircle, drawMyStrings, drawMySpells, drawMyAttacks, drawMyItems, drawHLine, drawRaText, showClass, showSubClass, statBonus, profBonus, saveBonus, skillBonus, spellsBy)

import Control.Monad ((>=>), forM_)
import Data.List (intercalate, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import Graphics.PDF

show0 :: Integer -> String
show0 0 = ""
show0 i = show i

show1 :: Integer -> String
show1 1 = ""
show1 i = show i

show4 :: Integer -> String
show4 (-4) = ""
show4 (-5) = ""
show4 i = show i

show5 :: Integer -> String
show5 (-5) = ""
-- show5 (-3) = ""
show5 i = show i

show6 :: Integer -> String 
show6 6 = ""
show6 x = show x

setupPage :: JpegFile -> Draw() -> PDF()
setupPage bg p = do
  page <- addPage Nothing
  jpg <- createPDFJpeg bg
  drawWithPage page $ do
    drawBackgroundImage jpg
    p

setupPage' :: Draw() -> PDF()
setupPage' p = do
  page <- addPage Nothing
  drawWithPage page p

drawBackgroundImage :: PDFReference PDFJpeg -> Draw ()
drawBackgroundImage jpg = 
  withNewContext $ do
    applyMatrix $ translate (0 :+ 0)
    applyMatrix $ scale 0.47 0.505
    drawXObject jpg

drawImage :: PDFFloat -> PDFFloat -> Maybe (PDFReference PDFJpeg) -> Draw()
drawImage x y mjpg =
  case mjpg of
    Nothing -> pure ()
    (Just jpg) -> do
      withNewContext $ do
        applyMatrix $ translate (x :+ y)
        drawXObject jpg

mkImage :: Maybe JpegFile -> PDF (Maybe (PDFReference PDFJpeg))
mkImage = maybe (pure Nothing) (createPDFJpeg >=> \ref -> pure (Just ref))

drawPage1 :: Character -> JpegFile -> Fonts -> PDF()
drawPage1 c bg fs = setupPage bg $ do
  drawMyText (fontLarge fs)  55 753 (characterName c) 
  drawMyText (fontSmall fs) 265 770 (showClass c)
  drawMyText (fontSmall fs) 265 741 (race c)
  drawMyText (fontSmall fs) 375 770 (background c)
  drawMyText (fontSmall fs) 375 741 (show $ alignment c)
  drawMyText (fontSmall fs) 470 770 (playerName c)
  drawMyText (fontSmall fs) 470 741 (experience c)
  drawMyText (fontLarge fs)  48 650 (show0 $ strength c)
  drawMyText (fontSmall fs)  53 626.5 (show5 $ statBonus $ strength c)
  drawMyText (fontLarge fs)  48 575 (show0 $ dexterity c)
  drawMyText (fontSmall fs)  53 551.5 (show5 $ statBonus $ dexterity c)
  drawMyText (fontLarge fs)  48 500 (show0 $ constitution c)
  drawMyText (fontSmall fs)  53 476.5 (show5 $ statBonus $ constitution c)
  drawMyText (fontLarge fs)  48 425 (show0 $ intelligence c)
  drawMyText (fontSmall fs)  53 401.5 (show5 $ statBonus $ intelligence c)
  drawMyText (fontLarge fs)  48 350 (show0 $ wisdom c)
  drawMyText (fontSmall fs)  53 326.5 (show5 $ statBonus $ wisdom c)
  drawMyText (fontLarge fs)  48 275 (show0 $ charisma c)
  drawMyText (fontSmall fs)  53 251.5 (show5 $ statBonus $ charisma c)
  drawMyText (fontLarge fs) 101 681 (show0 $ inspiration c)
  drawMyText (fontLarge fs) 102 642 (show1 $ profBonus c)
  drawMyText (fontLarge fs) 234.5 667 (calculatedArmourClass c)
  drawMyText (fontLarge fs) 293 667 (calculatedInitiative c)
  drawMyText (fontLarge fs) 346 667 (calculatedSpeed c)
  drawMyText (fontTiny fs)  290 618 (show0 $ calcHp c)
  drawMyText (fontTiny fs)  112 610 (show5 $ saveBonus (strength c) (strengthProf c) (strengthBonus c) (profBonus c))
  drawCircle 102 611 3 (fromIntegral $ strengthBonus c)
  drawMyText (fontTiny fs)  112 595 (show5 $ saveBonus (dexterity c) (dexterityProf c) (dexterityBonus c) (profBonus c))
  drawCircle 102 597 3 (fromIntegral $ dexterityBonus c)
  drawMyText (fontTiny fs)  112 581 (show5 $ saveBonus (constitution c) (constitutionProf c) (constitutionBonus c) (profBonus c))
  drawCircle 102 583 3 (fromIntegral $ constitutionBonus c)
  drawMyText (fontTiny fs)  112 567 (show5 $ saveBonus (intelligence c) (intelligenceProf c) (intelligenceBonus c) (profBonus c))
  drawCircle 102 568.5 3 (fromIntegral $ intelligenceBonus c)
  drawMyText (fontTiny fs)  112 552 (show5 $ saveBonus (wisdom c) (wisdomProf c) (wisdomBonus c) (profBonus c))
  drawCircle 102 554 3 (fromIntegral $ wisdomBonus c)
  drawMyText (fontTiny fs)  112 538 (show5 $ saveBonus (charisma c) (charismaProf c) (charismaBonus c) (profBonus c))
  drawCircle 102 540 3 (fromIntegral $ charismaBonus c)
  drawMyText (fontTiny fs)  112 488 (show5 $ skillBonus (dexterity c) (skillAcrobatics c) (profBonus c) (bonusAcrobatics c))
  drawCircle 102 490 3 (skillAcrobatics c)
  drawMyText (fontTiny fs)  112 473 (show5 $ skillBonus (wisdom c) (skillAnimalHandling c) (profBonus c) (bonusAnimalHandling c))
  drawCircle 102 475 3 (skillAnimalHandling c)
  drawMyText (fontTiny fs)  112 459 (show5 $ skillBonus (intelligence c) (skillArcana c) (profBonus c) (bonusArcana c))
  drawCircle 102 461 3 (skillArcana c)
  drawMyText (fontTiny fs)  112 445 (show5 $ skillBonus (strength c) (skillAthletics c) (profBonus c) (bonusAthletics c))
  drawCircle 102 447 3 (skillAthletics c)
  drawMyText (fontTiny fs)  112 431 (show5 $ skillBonus (charisma c) (skillDeception c) (profBonus c) (bonusDeception c))
  drawCircle 102 433 3 (skillDeception c)
  drawMyText (fontTiny fs)  112 416 (show5 $ skillBonus (intelligence c) (skillHistory c) (profBonus c) (bonusHistory c))
  drawCircle 102 418.5 3 (skillHistory c)
  drawMyText (fontTiny fs)  112 402 (show5 $ skillBonus (wisdom c) (skillInsight c) (profBonus c) (bonusInsight c))
  drawCircle 102 404.5 3 (skillInsight c)
  drawMyText (fontTiny fs)  112 388 (show5 $ skillBonus (charisma c) (skillIntimidation c) (profBonus c) (bonusIntimidation c))
  drawCircle 102 390 3 (skillIntimidation c)
  drawMyText (fontTiny fs)  112 374 (show5 $ skillBonus (intelligence c) (skillInvestigation c) (profBonus c) (bonusInvestigation c))
  drawCircle 102 376 3 (skillInvestigation c)
  drawMyText (fontTiny fs)  112 360 (show5 $ skillBonus (wisdom c) (skillMedicine c) (profBonus c) (bonusMedicine c))
  drawCircle 102 362 3 (skillMedicine c)
  drawMyText (fontTiny fs)  112 346 (show5 $ skillBonus (intelligence c) (skillNature c) (profBonus c) (bonusNature c))
  drawCircle 102 348 3 (skillNature c)
  drawMyText (fontTiny fs)  112 331 (show5 $ skillBonus (wisdom c) (skillPerception c) (profBonus c) (bonusPerception c))
  drawCircle 102 333 3 (skillPerception c)
  drawMyText (fontTiny fs)  112 317 (show5 $ skillBonus (charisma c) (skillPerformance c) (profBonus c) (bonusPerformance c))
  drawCircle 102 319 3 (skillPerformance c)
  drawMyText (fontTiny fs)  112 303 (show5 $ skillBonus (charisma c) (skillPersuasion c) (profBonus c) (bonusPersuasion c))
  drawCircle 102 305 3 (skillPersuasion c)
  drawMyText (fontTiny fs)  112 288 (show5 $ skillBonus (intelligence c) (skillReligion c) (profBonus c) (bonusReligion c))
  drawCircle 102 290.5 3 (skillReligion c)
  drawMyText (fontTiny fs)  112 274 (show5 $ skillBonus (dexterity c) (skillSleightOfHand c) (profBonus c) (bonusSleightOfHand c))
  drawCircle 102 276.5 3 (skillSleightOfHand c)
  drawMyText (fontTiny fs)  112 259 (show5 $ skillBonus (dexterity c) (skillStealth c) (profBonus c) (bonusStealth c))
  drawCircle 102 262 3 (skillStealth c)
  drawMyText (fontTiny fs)  112 246 (show5 $ skillBonus (wisdom c) (skillSurvival c) (profBonus c) (bonusSurvival c))
  drawCircle 102 248 3 (skillSurvival c)
  drawMyText (fontTiny fs)  250 490 (show0 $ level c)
  drawMyText (fontSmall fs)  250 468 (hitDice c)
  drawMyText (fontLarge fs)  34 198 (if wisdom c == 0 then "" else show (10 + statBonus (wisdom c)))
  drawMyText (fontSmall fs) 225 190 (show0 $ copper (moneyPouch c))
  drawMyText (fontSmall fs) 225 163 (show0 $ silver (moneyPouch c))
  drawMyText (fontSmall fs) 225 136 (show0 $ electrum (moneyPouch c))
  drawMyText (fontSmall fs) 225 109 (show0 $ gold (moneyPouch c))
  drawMyText (fontSmall fs) 225  81 (show0 $ platinum (moneyPouch c))
  drawMyStrings (fontTiny fs)   35 165 10 (proficiencies c)
  drawMyStrings (fontTiny fs)  410 675 10 (personalityTraits c)
  drawMyStrings (fontTiny fs)  410 602 10 (ideals c)
  drawMyStrings (fontTiny fs)  410 543 10 (bonds c)
  drawMyStrings (fontTiny fs)  410 485 10 (flaws c)
  drawMyStrings (fontTeeny fs) 405 416 10 (take 38 (features c))
  drawMyStrings (fontTiny fs) 265 200 10 (equipment c)
  drawMyAttacks (fontTiny fs) 222 410 21 (attacks c)

calculatedInitiative :: Character -> String
calculatedInitiative c = show4 $ statBonus (dexterity c) + initiative c

calculatedArmourClass :: Character -> String
calculatedArmourClass c = show6 $ acBase c + statBonus (dexterity c) + acBonus c

calculatedSpeed :: Character -> String
calculatedSpeed = speed

drawIcePage1 :: Character -> JpegFile -> Fonts -> PDF()
drawIcePage1 c bg fs = setupPage bg $ do
  drawCntText (fontLarge fs) 300 720 (characterName c) 
  drawMyText  (fontSmall fs)  32 795 (showClass c)
  drawMyText  (fontSmall fs)  32 784 (showSubClass c)
  drawMyText  (fontSmall fs)  32 718 (race c)
  drawRaText  (fontSmall fs) 565 784 (background c)
  drawMyText  (fontSmall fs)  32 752 (show $ alignment c)
  drawRaText  (fontSmall fs) 565 752 (playerName c)
  drawRaText  (fontSmall fs) 565 718 (experience c)
  -- Statistics and Stat Bonuses
  let stats = [charisma, wisdom, intelligence, constitution, dexterity, strength]
  forM_ (zip [0..] stats) $ \(y,f) -> do
    drawCntText (fontLarge fs) 54 (252 + y * 77) (show0 $ f c)
    drawCntText (fontSmall fs) 54 (224 + y * 77) (show5 $ statBonus $ f c)
  -- Inspiration, Prof Bonus, AC, Initiative Bonus, Speed
  drawCntText (fontLarge fs) 120 650 (show0 $ inspiration c)
  drawCntText (fontLarge fs) 175 650 (show1 $ profBonus c)
  drawCntText (fontLarge fs) 250 650 (calculatedArmourClass c)
  drawCntText (fontLarge fs) 297 650 (calculatedInitiative c)
  drawCntText (fontLarge fs) 348 650 (calculatedSpeed c)
  -- HP
  drawCntText (fontLarge fs) 240 572 (show0 $ calcHp c)
  drawMyText  (fontTiny fs)  267 597 (show $ hitPoints c)
  drawCntText (fontLarge fs) 240 510 (show0 $ tempHitPoints c)
  -- Saving throw bonuses and proficiency circles
  let bonuses = [ (charisma, charismaProf, charismaBonus)
                , (wisdom, wisdomProf, wisdomBonus)
                , (intelligence, intelligenceProf, intelligenceBonus)
                , (constitution, constitutionProf, constitutionBonus)
                , (dexterity, dexterityProf, dexterityBonus)
                , (strength, strengthProf, strengthBonus)]
  forM_ (zip [0..] bonuses) $ \(y, (sf,pf,bf)) -> do
    drawCntText (fontTiny fs) 120 (521 + y * 14.2) (show5 $ saveBonus (sf c) (pf c) (bf c) (profBonus c))
    drawCircle 105 (522 + y * 14.2) 3 (fromIntegral $ pf c)
  -- Skill bonuses and skill proficiency circles
  let skills = 
        [ (wisdom, skillSurvival, bonusSurvival)
        , (dexterity, skillStealth, bonusStealth)
        , (dexterity, skillSleightOfHand, bonusSleightOfHand)
        , (intelligence, skillReligion, bonusReligion)
        , (charisma, skillPersuasion, bonusPersuasion)
        , (charisma, skillPerformance, bonusPerformance)
        , (wisdom, skillPerception, bonusPerception)
        , (intelligence, skillNature, bonusNature)
        , (wisdom, skillMedicine, bonusMedicine)
        , (intelligence, skillInvestigation, bonusInvestigation)
        , (charisma, skillIntimidation, bonusIntimidation)
        , (wisdom, skillInsight, bonusInsight)
        , (intelligence, skillHistory, bonusHistory)
        , (charisma, skillDeception, bonusDeception)
        , (strength, skillAthletics, bonusAthletics)
        , (intelligence, skillArcana, bonusArcana)
        , (wisdom, skillAnimalHandling, bonusAnimalHandling)
        , (dexterity, skillAcrobatics, bonusAcrobatics)
        ]
  forM_ (zip [0..] skills) $ \(y,(statBonus, skillMultiple, skillB)) -> do
    drawCntText (fontTiny fs) 120 (230 + y * 14.75) (show5 $ skillBonus (statBonus c) (skillMultiple c) (profBonus c) (skillB c))
    drawCircle 104.8 (231.4 + y * 14.75) 3 (skillMultiple c)
  -- Hit dice
  drawCntText (fontNormal fs) 242 462 (show0 $ level c)
  drawMyText  (fontNormal fs) 262 462 (hitDice c)
  -- Passive perception
  drawCntText (fontSmall fs) 85 180 (passives c)
  -- Money 
  let monies = [copper, silver, electrum, gold, platinum]
  mapM_ (\(x,f) -> drawCntText (fontSmall fs) (231 + x * 34) 175 (show0 $ f (moneyPouch c))) (zip [0..] monies)
  -- Proficiencies and Languges
  drawMyStrings (fontTiny fs)   35 144.8 13.6 (proficiencies c)
  -- Traits, Ideals, Bonds and Flaws
  drawMyStrings (fontTiny fs)  410 667 13.6 (personalityTraits c)
  drawMyStrings (fontTiny fs)  410 607 13.6 (ideals c)
  drawMyStrings (fontTiny fs)  410 547 13.6 (bonds c)
  drawMyStrings (fontTiny fs)  410 487 13.6 (flaws c)
  -- Features and Traits
  drawMyStrings (fontTeeny fs) 405 408 13.3 (take 29 (features c))
  -- Equipment
  drawMyStrings (fontTiny fs) 222 144.8 13.6 (equipment c)
  -- Attacks
  drawMyAttacks (fontTiny fs) 222 395 21 (take 3 $ attacks c)
  drawMyAttacks (fontTiny fs) 222 330 13 (drop 3 $ attacks c)

passives :: Character -> String
passives c = "P[" <> show passivePerception <> "] Is[" <> show passiveInsight <> "] Iv[" <> show passiveInvestigation <> "]"
  where
    passivePerception = 10 + skillBonus (wisdom c) (skillPerception c) (profBonus c) (bonusPassivePerception c)
    passiveInsight = 10 + skillBonus (wisdom c) (skillInsight c) (profBonus c) (bonusPassiveInsight c)
    passiveInvestigation = 10 + skillBonus (intelligence c) (skillInvestigation c) (profBonus c) (bonusPassiveInvestigation c)

drawPage2 :: Character -> JpegFile -> Maybe JpegFile -> Fonts -> PDF()
drawPage2 c bg mapp fs = do
  app <- mkImage mapp
  setupPage bg $ do
    drawMyText (fontLarge fs)  55 751 (characterName c) 
    drawMyText (fontSmall fs) 265 767 (age c)
    drawMyText (fontSmall fs) 265 738 (eyeColour c)
    drawMyText (fontSmall fs) 370 767 (height c)
    drawMyText (fontSmall fs) 370 738 (skinColour c)
    drawMyText (fontSmall fs) 465 767 (weight c)
    drawMyText (fontSmall fs) 465 738 (hairColour c)
    drawImage 35 490 app
    drawMyStrings (fontTeeny fs)  35 423 11.5 (backstory c)
    drawMyStrings (fontTeeny fs) 225 687 11.5 (allies c)
    drawMyStrings (fontTeeny fs) 225 433 11.5 (take 19 (drop 38 (features c)))
    drawMyStrings (fontTeeny fs) 405 433 11.5 (drop 57 (features c))
    drawMyStrings (fontTeeny fs) 225 190 11.5 (take 14 (treasure c))
    drawMyStrings (fontTeeny fs) 405 190 11.5 (drop 14 (treasure c))

drawIcePage2 :: Character -> JpegFile -> Maybe JpegFile -> Fonts -> PDF()
drawIcePage2 c bg mapp fs = do
  app <- mkImage mapp
  setupPage bg $ do
    drawCntText (fontLarge fs) 180 760 (characterName c) 
    drawMyText  (fontSmall fs) 312 775 (age c)
    drawMyText  (fontSmall fs) 312 741 (eyeColour c)
    drawMyText  (fontSmall fs) 406 775 (height c)
    drawMyText  (fontSmall fs) 406 741 (skinColour c)
    drawMyText  (fontSmall fs) 500 775 (weight c)
    drawMyText  (fontSmall fs) 500 741 (hairColour c)
    drawImage 35 490 app
    -- drawMyStrings (fontTeeny fs)  37 417 13.2 (backstory c)
    drawTxtBox (helvetica fs) 35 30 162 395 (unlines $ backstory c)
    drawMyStrings (fontTeeny fs) 225 690 13.2 (take 18 (allies c))
    drawMyStrings (fontTeeny fs) 405 518 13.2 (drop 18 (allies c))
    drawMyStrings (fontTeeny fs) 225 425 13.2 (take 16 (drop 29 (features c)))
    drawMyStrings (fontTeeny fs) 405 425 13.2 (take 16 (drop 45 (features c)))
    drawMyStrings (fontTeeny fs) 225 183 13.2 (take 12 (drop 61 (features c)))
    drawMyStrings (fontTeeny fs) 405 183 13.2 (drop 73 (features c))
    drawMyStrings (fontTeeny fs) 405 183 13.2 (treasure c)

drawSpellPage :: JpegFile -> Fonts -> Spellcasting -> PDF()
drawSpellPage bg fs c = setupPage bg $ do
  let spells0 = spellsBy Cantrip (knownSpells c)
  let spells1 = spellsBy One (knownSpells c)
  let spells2 = spellsBy Two (knownSpells c)
  let spells3 = spellsBy Three (knownSpells c)
  let spells4 = spellsBy Four (knownSpells c)
  let spells5 = spellsBy Five (knownSpells c)
  let spells6 = spellsBy Six (knownSpells c)
  let spells7 = spellsBy Seven (knownSpells c)
  let spells8 = spellsBy Eight (knownSpells c)
  let spells9 = spellsBy Nine (knownSpells c)
  drawMyText (fontLarge fs)  70 753 (spellcastingClass c) 
  drawMyText (fontLarge fs) 278 757 (spellcastingAbility c) 
  drawMyText (fontLarge fs) 400 757 (spellSaveDC c) 
  drawMyText (fontLarge fs) 505 757 (spellAttackBonus c) 
  drawMySpells (fontTiny fs)   50 643 14.7 (take 8 spells0)
  drawMySpells (fontTiny fs)  140 643 14.7 (take 8 $ drop 8 spells0)
  drawMySpells (fontTiny fs)  420 284 14.7 (take 7 $ drop 16 spells0)
  drawMySpells (fontTiny fs)  510 284 14.7 (take 7 $ drop 23 spells0)
  drawMySpells (fontTiny fs)  420 136 14.7 (take 7 $ drop 30 spells0)
  drawMySpells (fontTiny fs)  510 136 14.7 (drop 37 spells0)
  drawMySpells (fontTiny fs)   50 461 14.7 (take 13 spells1)
  drawMySpells (fontTiny fs)  140 461 14.7 (drop 13 spells1)
  drawMySpells (fontTiny fs)   50 224 14.7 (take 13 spells2)
  drawMySpells (fontTiny fs)  140 224 14.7 (drop 13 spells2)
  drawMySpells (fontTiny fs)  235 642 14.7 (take 13 spells3)
  drawMySpells (fontTiny fs)  325 642 14.7 (drop 13 spells3)
  drawMySpells (fontTiny fs)  235 404 14.7 (take 13 spells4)
  drawMySpells (fontTiny fs)  325 404 14.7 (drop 13 spells4)
  drawMySpells (fontTiny fs)  235 165 14.7 (take 9 spells5)
  drawMySpells (fontTiny fs)  325 165 14.7 (drop 9 spells5)
  drawMySpells (fontTiny fs)  420 642 14.7 (take 9 spells6)
  drawMySpells (fontTiny fs)  510 642 14.7 (drop 9 spells6)
  drawMySpells (fontTiny fs)  420 463 14.7 (take 9 spells7)
  drawMySpells (fontTiny fs)  510 463 14.7 (drop 9 spells7)
  drawMySpells (fontTiny fs)  420 284 14.7 (take 7 spells8)
  drawMySpells (fontTiny fs)  510 284 14.7 (drop 7 spells8)
  drawMySpells (fontTiny fs)  420 136 14.7 (take 7 spells9)
  drawMySpells (fontTiny fs)  510 136 14.7 (drop 7 spells9)
  drawMyText    (fontSmall fs)  65 489    (slotsLevel1 c)
  drawMyText    (fontSmall fs)  65 249    (slotsLevel2 c)
  drawMyText    (fontSmall fs) 250 667    (slotsLevel3 c)
  drawMyText    (fontSmall fs) 250 429    (slotsLevel4 c)
  drawMyText    (fontSmall fs) 250 190    (slotsLevel5 c)
  drawMyText    (fontSmall fs) 435 667    (slotsLevel6 c)
  drawMyText    (fontSmall fs) 435 488    (slotsLevel7 c)
  drawMyText    (fontSmall fs) 435 309    (slotsLevel8 c)
  drawMyText    (fontSmall fs) 435 161    (slotsLevel9 c)

drawIceSpellPage :: JpegFile -> Fonts -> Spellcasting -> PDF()
drawIceSpellPage bg fs c = setupPage bg $ do
  let spells0 = spellsBy Cantrip (knownSpells c)
  let spells1 = spellsBy One (knownSpells c)
  let spells2 = spellsBy Two (knownSpells c)
  let spells3 = spellsBy Three (knownSpells c)
  let spells4 = spellsBy Four (knownSpells c)
  let spells5 = spellsBy Five (knownSpells c)
  let spells6 = spellsBy Six (knownSpells c)
  let spells7 = spellsBy Seven (knownSpells c)
  let spells8 = spellsBy Eight (knownSpells c)
  let spells9 = spellsBy Nine (knownSpells c)
  drawCntText (fontLarge fs) 180 760 (spellcastingClass c) 
  drawCntText (fontLarge fs) 348 760 (spellcastingAbility c) 
  drawCntText (fontLarge fs) 435 760 (spellSaveDC c) 
  drawCntText (fontLarge fs) 520 760 (spellAttackBonus c) 
  drawCntText (fontLarge fs)  43 488 "1"
  drawMySpells (fontTiny fs)   50 643.0 14.7 (take 8 spells0)
  drawMySpells (fontTiny fs)  140 643.0 14.7 (take 8 $ drop 8 spells0)
  drawMySpells (fontTiny fs)  420 282.3 14.7 (take 8 $ drop 16 spells0)
  drawMySpells (fontTiny fs)  510 282.3 14.7 (take 8 $ drop 24 spells0)
  drawMySpells (fontTiny fs)  420 134.5 14.7 (take 9 $ drop 32 spells0)
  drawMySpells (fontTiny fs)  510 134.5 14.7 (drop 41 spells0)
  drawMySpells (fontTiny fs)   50 446.1 14.7 (take 12 spells1)
  drawMySpells (fontTiny fs)  140 446.1 14.7 (drop 12 spells1)
  drawMySpells (fontTiny fs)   50 220.5 14.7 (take 13 spells2)   
  drawMySpells (fontTiny fs)  140 220.5 14.7 (drop 13 spells2)   
  drawMySpells (fontTiny fs)  235 637.8 14.7 (take 13 spells3)
  drawMySpells (fontTiny fs)  325 637.8 14.7 (drop 13 spells3)
  drawMySpells (fontTiny fs)  235 402.0 14.7 (take 13 spells4)
  drawMySpells (fontTiny fs)  325 402.0 14.7 (drop 13 spells4)
  drawMySpells (fontTiny fs)  235 165.0 14.7 (take 9 spells5)
  drawMySpells (fontTiny fs)  325 165.0 14.7 (drop 9 spells5)
  drawMySpells (fontTiny fs)  420 638.0 14.7 (take 9 spells6)   
  drawMySpells (fontTiny fs)  510 638.0 14.7 (drop 9 spells6)   
  drawMySpells (fontTiny fs)  420 461.0 14.7 (take 9 spells7)
  drawMySpells (fontTiny fs)  510 461.0 14.7 (drop 9 spells7)
  drawMySpells (fontTiny fs)  420 282.3 14.7 (take 7 spells8)
  drawMySpells (fontTiny fs)  510 282.3 14.7 (drop 7 spells8)
  drawMySpells (fontTiny fs)  420 134.5 14.7 (take 7 spells9)
  drawMySpells (fontTiny fs)  510 134.5 14.7 (drop 7 spells9)
  drawCntText    (fontSmall fs)  85 489    (slotsLevel1 c)
  drawCntText    (fontSmall fs)  85 249    (slotsLevel2 c)
  drawCntText    (fontSmall fs) 270 667    (slotsLevel3 c)
  drawCntText    (fontSmall fs) 270 429    (slotsLevel4 c)
  drawCntText    (fontSmall fs) 270 190    (slotsLevel5 c)
  drawCntText    (fontSmall fs) 455 667    (slotsLevel6 c)
  drawCntText    (fontSmall fs) 455 488    (slotsLevel7 c)
  drawCntText    (fontSmall fs) 455 309    (slotsLevel8 c)
  drawCntText    (fontSmall fs) 455 161    (slotsLevel9 c)

drawBackpackPage :: JpegFile -> Character -> Fonts -> Backpack -> PDF()
drawBackpackPage bg c fs b = setupPage bg $ do
  drawMyText    (fontLarge fs) 55 751      (characterName c)
  drawMyStrings (fontTiny fs) 235 685 12.5 (bagPocket1 b)
  drawMyStrings (fontTiny fs) 235 611 12.5 (bagPocket2 b)
  drawMyStrings (fontTiny fs) 235 555.5 12.5 (bagPocket3 b)
  drawMyStrings (fontTiny fs) 235 494.5 12.5 (bagPocket4 b)
  drawMyStrings (fontTiny fs) 405 680 11.5 (bagFlapPouch b)
  drawMyStrings (fontTiny fs) 405 584 11.5 (bagMiddlePouch b)
  drawMyStrings (fontTiny fs) 232 431 11.5 (take 34 $ bagMainPouch b)
  drawMyStrings (fontTiny fs) 405 431 11.5 (drop 34 $ bagMainPouch b)
  drawMyText    (fontTiny fs)  75 329.1    (show0 $ copper $ bagCash b)
  drawMyText    (fontTiny fs)  75 306.1    (show0 $ silver $ bagCash b)
  drawMyText    (fontTiny fs)  75 283.1    (show0 $ electrum $ bagCash b)
  drawMyText    (fontTiny fs)  75 260.1    (show0 $ gold $ bagCash b)
  drawMyText    (fontTiny fs)  75 237.1    (show0 $ platinum $ bagCash b)
  drawMyStrings (fontTiny fs)  35 190.5 11.5 (gems $ bagCash b)
  drawMyText    (fontTiny fs)  75 408.7    (bagBedroll b)
  drawMyText    (fontTiny fs)  75 397.2    (bagRope b)
  drawMyText    (fontTiny fs)  75 385.7    (bagAmmo b)
  drawMyText    (fontTiny fs)  75 374.2    (bagTorches b)
  drawMyStrings (fontTiny fs)  35 189.5 11.5 (bagTreasure b)


drawBagOfHolding :: JpegFile -> Character -> Fonts -> BagOfHolding -> PDF()
drawBagOfHolding bg c fs b = setupPage bg $ do
  let rowHeight = 11.8
  drawMyText (fontLarge fs)  55 765 (characterName c)
  drawMyItems (fontTiny fs)  35 130 150 500 rowHeight (bohPGs b)
  drawMyItems (fontTiny fs) 190 338 358 674.5 rowHeight (take 48 $ bohItems b)
  drawMyItems (fontTiny fs) 383 532 553 674.5 rowHeight (drop 48 $ bohItems b)
      
drawPortableHole :: JpegFile -> Character -> Fonts -> PortableHole -> PDF()
drawPortableHole bg c fs b = setupPage bg $ do
  let lineSpacing = 11.6
  drawMyText (fontLarge fs)  55 750 (characterName c) 
  drawMyStrings (fontTiny fs)  45.3 694 lineSpacing (take 50 $ phItems b)
  drawMyStrings (fontTiny fs) 230 508 lineSpacing (take 34 $ drop 50 $ phItems b)
  drawMyStrings (fontTiny fs) 405 694 lineSpacing (drop 84 $ phItems b)

-- drawAbilities :: Character -> Maybe [[AbilityGroup]] -> Fonts -> PDF()
-- drawAbilities _ Nothing   _  = pure ()
-- drawAbilities c (Just as) fs = drawAbilities' c as fs

-- drawAbilities' :: Character -> [[AbilityGroup]] -> Fonts -> PDF()
-- drawAbilities' _ [] _ = pure ()
-- drawAbilities' c (a:as) fs = do
--   page <- addPage Nothing
--   drawWithPage page $ do
--     drawMyText (fontLarge fs) 40 780 "Abilities"
--     drawAbilityGroups 760 a fs
--   drawAbilities' c as fs

-- drawAbilityGroups :: PDFFloat -> [AbilityGroup] -> Fonts -> Draw()
-- drawAbilityGroups _ [] _ = pure()
-- drawAbilityGroups i (a:as) fs = do
--     i' <- drawAbilityGroup i a fs
--     drawAbilityGroups i' as fs

-- drawAbilityGroup :: PDFFloat -> AbilityGroup -> Fonts -> Draw PDFFloat
-- drawAbilityGroup i a fs = do
--     -- draw a horizontal line
--     drawHLine (i + 12)
--     drawMyText (fontSmall fs) 40 i $ groupName a
--     drawMyText (fontSmall fs) 360 i $ "HD: " ++ show (groupHitDice a)
--     drawMyText (fontSmall fs) 440 i $ "Count: " ++ show (groupCount a)
--     drawMyStrings (fontTiny fs) 45 (i - 11.5) 11.5 (groupAbilities a)
--     let h = 11.5 * fromIntegral (length (groupAbilities a)) :: PDFFloat
--     return (i - h - 20)

data MyVertStyles = NormalPara

instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True

instance ParagraphStyle MyVertStyles MyParaStyles  where
    lineWidth _ w _ = w
    linePosition _ _ _ = 0.0
    interline _ = Nothing
    paragraphChange s _ l = (s,l)
    paragraphStyle _ = Nothing

data MyParaStyles = Normal AnyFont

instance ComparableStyle MyParaStyles where
  isSameStyleAs (Normal fa) (Normal fb) = fa == fb

instance Style MyParaStyles where
    textStyle (Normal f) = TextStyle (PDFFont f 10) black black FillText 1.0 1.0 1.0 1.0
    wordStyle _ = Nothing

htmlSpells :: Character -> String
htmlSpells ch =  "<html>\n\t<style>"
              <> "\n\t\tbody { font-family: Verdana, sans-serif; font-size: 12px; }"
              <> "\n\t\th2 { font-family: Copperplate, fantasy; color: Sienna; font-size: 32px; } "
              <> "\n\t\tdiv.spellblock { padding-bottom: 0px; } "
              <> "\n\t\tspan.title { font-family: Copperplate, fantasy; color: Sienna; font-size: 18px; } "
              <> "\n\t\tspan.spelltype { font-style: italic; font-size: 10px; }"
              <> "\n\t\tspan.header { font-weight: bold; }"
              <> "\n\t\ttable { font-family: Verdana, sans-serif; font-size: 12px; } "
              <> "\n\t\ttable, th, td { border: 1px solid black; border-collapse: collapse; } "
              <> "\n\t\tth, td { padding-left: 10px; padding-right: 10px; } "
              <> "\n\t\ttd {text-align: center; }"
              <> "\n\t</style>\n<body>"
              <> "\n\t<h2>Spellbook for " <> characterName ch <> "</h2>\n\t<hr/>"
              <> intercalate "\n\t<hr/>" (mapMaybe (htmlKnownSpell Nine) ss)
              <> "\n</body>\n</html>"
  where
    cs = spellcasting ch
    ks = concatMap knownSpells cs
    ss = sortOn spellName ks

htmlKnownSpell :: SpellLevel -> KnownSpell -> Maybe String
htmlKnownSpell lvl ks = 
  case getSpell (spellName ks) of
    Nothing -> Nothing
    (Just s) -> if (spLevel s <= lvl)
                then Just $ htmlSpell ks s
                else Nothing

htmlSpell :: KnownSpell -> Spell -> String
htmlSpell k s = "<div class=\"spellblock\"><p>"
              <> "<span class=\"title\">" <> spName s <> "</span><br/>"
              <> "<span class=\"spelltype\">" <> spellTypeLine (spType s) (spLevel s) <> "</span><br/>"
              <> "<span class=\"header\">Casting Time:&nbsp;</span>" <> spTime s <> "<br/>"
              <> "<span class=\"header\">Range:&nbsp;</span>" <> spRange s <> "<br/>"
              <> "<span class=\"header\">Components:&nbsp;</span>" <> spComponents s <> "<br/>"
              <> "<span class=\"header\">Duration:&nbsp;</span>" <> spDuration s <> "<br/>"
              <> "</p>"
              <> "<p>" <> spDescription s <> "</p>"
              <> htmlHigher (spHigher s)
              <> "</div>"

htmlHigher :: Maybe String -> String
htmlHigher Nothing = ""
htmlHigher (Just h) = "<p><b>At Higher Levels:</b> " <> h <> "</p>"

spellTypeLine :: SpellType -> SpellLevel -> String
spellTypeLine t Cantrip = spellType t <> " Cantrip"
spellTypeLine t l = spellLevel l <> "-level " <> spellType t

spellType :: SpellType -> String
spellType Abjuration  = "Abjuration"
spellType Conjuration = "Conjuration"
spellType Divination = "Divination"
spellType Enchantment = "Enchantment"
spellType Evocation = "Evocation"
spellType Illusion = "Illusion"
spellType Necromancy = "Necromancy"
spellType Transmutation = "Transmutation"
spellType SpellTypeUnknown = "SpellTypeUnknown"

spellLevel :: SpellLevel -> String
spellLevel Cantrip = "Cantrip"
spellLevel One = "1st"
spellLevel Two = "2nd"
spellLevel Three = "3rd"
spellLevel Four = "4th"
spellLevel Five = "5th"
spellLevel Six = "6th"
spellLevel Seven = "7th"
spellLevel Eight = "8th"
spellLevel Nine = "9th"
spellLevel SpellLevelUnknown = "SpellLevelUnknown"

calcHp :: Character -> Integer
calcHp c = sum (map (\d -> d + statBonus (constitution c)) (hitPoints c))

    