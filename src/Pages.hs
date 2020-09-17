module Pages where

import DataTypes (Character, Spellcasting, Backpack, BagOfHolding, PortableHole, AbilityGroup,
    characterName, race, background, alignment, playerName, experience, 
    strength, dexterity, constitution, intelligence, wisdom, charisma,
    inspiration, acBase, acBonus, initiative, speed, hitPoints, 
    strengthBonus, dexterityBonus, constitutionBonus, intelligenceBonus, wisdomBonus, charismaBonus,
    skillAcrobatics, skillAnimalHandling, skillArcana, skillDeception, skillHistory, skillInsight, 
    skillIntimidation, skillInvestigation, skillMedicine, skillNature, skillPerception, skillPerformance, 
    skillPersuasion, skillReligion, skillSleightOfHand, skillStealth, skillSurvival, skillAthletics,
    level, hitDice, copper, moneyPouch, silver, electrum, gold, platinum, gems, proficiencies, personalityTraits, ideals, bonds, flaws, features, equipment, attacks,
    age, eyeColour, height, skinColour, weight, hairColour, backstory, allies, treasure,
    spellcastingClass, spellcastingAbility, spellSaveDC, spellAttackBonus, spellsCantrips, 
    spellsLevel1, spellsLevel2, spellsLevel3, spellsLevel4, spellsLevel5, spellsLevel6, spellsLevel7, spellsLevel8, spellsLevel9, 
    slotsLevel1, slotsLevel2, slotsLevel3, slotsLevel4, slotsLevel5, slotsLevel6, slotsLevel7, slotsLevel8, slotsLevel9,
    bagPocket1, bagPocket2, bagPocket3, bagPocket4, bagFlapPouch, bagMiddlePouch, bagMainPouch, bagCash, bagBedroll, bagRope, bagAmmo, bagTorches, bagTreasure,
    bohPGs, bohItems, phItems,
    groupName, groupHitDice, groupCount, groupAbilities)
import Fonts(fntLarge, fntSmall, fntTiny, fntTeeny)
import Utilities(drawMyText, drawCircle, drawMyStrings, drawMyAttacks, drawMyItems, drawHLine, showClass, statBonus, profBonus, saveBonus, skillBonus)

import Graphics.PDF(Draw, JpegFile, PDF, PDFFloat, PDFJpeg, PDFReference, addPage, createPDFJpeg, drawWithPage, withNewContext, applyMatrix, translate, Complex(..), scale, drawXObject)

show0 :: Integer -> String
show0 0 = ""
show0 i = show i

show1 :: Integer -> String
show1 1 = ""
show1 i = show i

show5 :: Integer -> String
show5 (-5) = ""
show5 i = show i

setupPage :: JpegFile -> Draw() -> PDF()
setupPage bg p = do
  page <- addPage Nothing
  jpg <- createPDFJpeg bg
  drawWithPage page $ do
    drawBackgroundImage jpg
    p

drawBackgroundImage :: PDFReference PDFJpeg -> Draw ()
drawBackgroundImage jpg = 
  withNewContext $ do
    applyMatrix $ translate (0 :+ 0)
    applyMatrix $ scale 0.47 0.505
    drawXObject jpg

drawPage1 :: Character -> JpegFile -> PDF()
drawPage1 c bg = setupPage bg $ do
  drawMyText fntLarge  55 753 (characterName c) 
  drawMyText fntSmall 265 770 (showClass c)
  drawMyText fntSmall 265 741 (race c)
  drawMyText fntSmall 375 770 (background c)
  drawMyText fntSmall 375 741 (show $ alignment c)
  drawMyText fntSmall 470 770 (playerName c)
  drawMyText fntSmall 470 741 (show0 $ experience c)
  drawMyText fntLarge  48 650 (show0 $ strength c)
  drawMyText fntSmall  53 626.5 (show5 $ statBonus $ strength c)
  drawMyText fntLarge  48 575 (show0 $ dexterity c)
  drawMyText fntSmall  53 551.5 (show5 $ statBonus $ dexterity c)
  drawMyText fntLarge  48 500 (show0 $ constitution c)
  drawMyText fntSmall  53 476.5 (show5 $ statBonus $ constitution c)
  drawMyText fntLarge  48 425 (show0 $ intelligence c)
  drawMyText fntSmall  53 401.5 (show5 $ statBonus $ intelligence c)
  drawMyText fntLarge  48 350 (show0 $ wisdom c)
  drawMyText fntSmall  53 326.5 (show5 $ statBonus $ wisdom c)
  drawMyText fntLarge  48 275 (show0 $ charisma c)
  drawMyText fntSmall  53 251.5 (show5 $ statBonus $ charisma c)
  drawMyText fntLarge 101 681 (show5 $ inspiration c)
  drawMyText fntLarge 102 642 (show1 $ profBonus c)
  drawMyText fntLarge 234.5 667 (show5 $ acBase c + statBonus (dexterity c) + acBonus c)
  drawMyText fntLarge 293 667 (show5 $ statBonus (dexterity c) + initiative c + profBonus c)
  drawMyText fntLarge 346 667 (if speed c == 0 then "" else show (speed c) ++ "'")
  drawMyText fntTiny  290 618 (show0 $ hitPoints c)
  drawMyText fntTiny  112 610 (show5 $ saveBonus (strength c) (strengthBonus c) (profBonus c))
  drawCircle 102 611 3 (strengthBonus c)
  drawMyText fntTiny  112 595 (show5 $ saveBonus (dexterity c) (dexterityBonus c) (profBonus c))
  drawCircle 102 597 3 (dexterityBonus c)
  drawMyText fntTiny  112 581 (show5 $ saveBonus (constitution c) (constitutionBonus c) (profBonus c))
  drawCircle 102 583 3 (constitutionBonus c)
  drawMyText fntTiny  112 567 (show5 $ saveBonus (intelligence c) (intelligenceBonus c) (profBonus c))
  drawCircle 102 568.5 3 (intelligenceBonus c)
  drawMyText fntTiny  112 552 (show5 $ saveBonus (wisdom c) (wisdomBonus c) (profBonus c))
  drawCircle 102 554 3 (wisdomBonus c)
  drawMyText fntTiny  112 538 (show5 $ saveBonus (charisma c) (charismaBonus c) (profBonus c))
  drawCircle 102 540 3 (charismaBonus c)
  drawMyText fntTiny  112 488 (show5 $ skillBonus (dexterity c) (dexterityBonus c) (skillAcrobatics c) (profBonus c))
  drawCircle 102 490 3 (skillAcrobatics c)
  drawMyText fntTiny  112 473 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillAnimalHandling c) (profBonus c))
  drawCircle 102 475 3 (skillAnimalHandling c)
  drawMyText fntTiny  112 459 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillArcana c) (profBonus c))
  drawCircle 102 461 3 (skillArcana c)
  drawMyText fntTiny  112 445 (show5 $ skillBonus (strength c) (strengthBonus c) (skillAthletics c) (profBonus c))
  drawCircle 102 447 3 (skillAthletics c)
  drawMyText fntTiny  112 431 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillDeception c) (profBonus c))
  drawCircle 102 433 3 (skillDeception c)
  drawMyText fntTiny  112 416 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillHistory c) (profBonus c))
  drawCircle 102 418.5 3 (skillHistory c)
  drawMyText fntTiny  112 402 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillInsight c) (profBonus c))
  drawCircle 102 404.5 3 (skillInsight c)
  drawMyText fntTiny  112 388 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillIntimidation c) (profBonus c))
  drawCircle 102 390 3 (skillIntimidation c)
  drawMyText fntTiny  112 374 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillInvestigation c) (profBonus c))
  drawCircle 102 376 3 (skillInvestigation c)
  drawMyText fntTiny  112 360 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillMedicine c) (profBonus c))
  drawCircle 102 362 3 (skillMedicine c)
  drawMyText fntTiny  112 346 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillNature c) (profBonus c))
  drawCircle 102 348 3 (skillNature c)
  drawMyText fntTiny  112 331 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillPerception c) (profBonus c))
  drawCircle 102 333 3 (skillPerception c)
  drawMyText fntTiny  112 317 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillPerformance c) (profBonus c))
  drawCircle 102 319 3 (skillPerformance c)
  drawMyText fntTiny  112 303 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillPersuasion c) (profBonus c))
  drawCircle 102 305 3 (skillPersuasion c)
  drawMyText fntTiny  112 288 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillReligion c) (profBonus c))
  drawCircle 102 290.5 3 (skillReligion c)
  drawMyText fntTiny  112 274 (show5 $ skillBonus (dexterity c) (dexterityBonus c) (skillSleightOfHand c) (profBonus c))
  drawCircle 102 276.5 3 (skillSleightOfHand c)
  drawMyText fntTiny  112 259 (show5 $ skillBonus (dexterity c) (dexterityBonus c) (skillStealth c) (profBonus c))
  drawCircle 102 262 3 (skillStealth c)
  drawMyText fntTiny  112 246 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillSurvival c) (profBonus c))
  drawCircle 102 248 3 (skillSurvival c)
  drawMyText fntTiny  250 490 (show0 $ level c)
  drawMyText fntSmall  250 468 (if hitDice c == 0 then "" else "d" ++ show (hitDice c))
  drawMyText fntLarge  34 198 (if wisdom c == 0 then "" else show (10 + statBonus (wisdom c)))
  drawMyText fntSmall 225 190 (show0 $ copper (moneyPouch c))
  drawMyText fntSmall 225 163 (show0 $ silver (moneyPouch c))
  drawMyText fntSmall 225 136 (show0 $ electrum (moneyPouch c))
  drawMyText fntSmall 225 109 (show0 $ gold (moneyPouch c))
  drawMyText fntSmall 225  81 (show0 $ platinum (moneyPouch c))
  drawMyStrings fntTiny   35 165 10 (proficiencies c)
  drawMyStrings fntTiny  410 675 10 (personalityTraits c)
  drawMyStrings fntTiny  410 602 10 (ideals c)
  drawMyStrings fntTiny  410 543 10 (bonds c)
  drawMyStrings fntTiny  410 485 10 (flaws c)
  drawMyStrings fntTeeny 405 416 10 (take 38 (features c))
  drawMyStrings fntTiny 265 200 10 (equipment c)
  drawMyAttacks fntTiny 222 410 21 (attacks c)

drawIcePage1 :: Character -> JpegFile -> PDF()
drawIcePage1 c bg = setupPage bg $ do
  drawMyText fntLarge  55 753 (characterName c) 
  drawMyText fntSmall 265 770 (showClass c)
  drawMyText fntSmall 265 741 (race c)
  drawMyText fntSmall 375 770 (background c)
  drawMyText fntSmall 375 741 (show $ alignment c)
  drawMyText fntSmall 470 770 (playerName c)
  drawMyText fntSmall 470 741 (show0 $ experience c)
  drawMyText fntLarge  48 650 (show0 $ strength c)
  drawMyText fntSmall  53 626.5 (show5 $ statBonus $ strength c)
  drawMyText fntLarge  48 575 (show0 $ dexterity c)
  drawMyText fntSmall  53 551.5 (show5 $ statBonus $ dexterity c)
  drawMyText fntLarge  48 500 (show0 $ constitution c)
  drawMyText fntSmall  53 476.5 (show5 $ statBonus $ constitution c)
  drawMyText fntLarge  48 425 (show0 $ intelligence c)
  drawMyText fntSmall  53 401.5 (show5 $ statBonus $ intelligence c)
  drawMyText fntLarge  48 350 (show0 $ wisdom c)
  drawMyText fntSmall  53 326.5 (show5 $ statBonus $ wisdom c)
  drawMyText fntLarge  48 275 (show0 $ charisma c)
  drawMyText fntSmall  53 251.5 (show5 $ statBonus $ charisma c)
  drawMyText fntLarge 101 681 (show5 $ inspiration c)
  drawMyText fntLarge 102 642 (show1 $ profBonus c)
  drawMyText fntLarge 234.5 667 (show5 $ acBase c + statBonus (dexterity c) + acBonus c)
  drawMyText fntLarge 293 667 (show5 $ statBonus (dexterity c) + initiative c + profBonus c)
  drawMyText fntLarge 346 667 (if speed c == 0 then "" else show (speed c) ++ "'")
  drawMyText fntTiny  290 618 (show0 $ hitPoints c)
  drawMyText fntTiny  112 610 (show5 $ saveBonus (strength c) (strengthBonus c) (profBonus c))
  drawCircle 102 611 3 (strengthBonus c)
  drawMyText fntTiny  112 595 (show5 $ saveBonus (dexterity c) (dexterityBonus c) (profBonus c))
  drawCircle 102 597 3 (dexterityBonus c)
  drawMyText fntTiny  112 581 (show5 $ saveBonus (constitution c) (constitutionBonus c) (profBonus c))
  drawCircle 102 583 3 (constitutionBonus c)
  drawMyText fntTiny  112 567 (show5 $ saveBonus (intelligence c) (intelligenceBonus c) (profBonus c))
  drawCircle 102 568.5 3 (intelligenceBonus c)
  drawMyText fntTiny  112 552 (show5 $ saveBonus (wisdom c) (wisdomBonus c) (profBonus c))
  drawCircle 102 554 3 (wisdomBonus c)
  drawMyText fntTiny  112 538 (show5 $ saveBonus (charisma c) (charismaBonus c) (profBonus c))
  drawCircle 102 540 3 (charismaBonus c)
  drawMyText fntTiny  112 488 (show5 $ skillBonus (dexterity c) (dexterityBonus c) (skillAcrobatics c) (profBonus c))
  drawCircle 102 490 3 (skillAcrobatics c)
  drawMyText fntTiny  112 473 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillAnimalHandling c) (profBonus c))
  drawCircle 102 475 3 (skillAnimalHandling c)
  drawMyText fntTiny  112 459 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillArcana c) (profBonus c))
  drawCircle 102 461 3 (skillArcana c)
  drawMyText fntTiny  112 445 (show5 $ skillBonus (strength c) (strengthBonus c) (skillAthletics c) (profBonus c))
  drawCircle 102 447 3 (skillAthletics c)
  drawMyText fntTiny  112 431 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillDeception c) (profBonus c))
  drawCircle 102 433 3 (skillDeception c)
  drawMyText fntTiny  112 416 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillHistory c) (profBonus c))
  drawCircle 102 418.5 3 (skillHistory c)
  drawMyText fntTiny  112 402 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillInsight c) (profBonus c))
  drawCircle 102 404.5 3 (skillInsight c)
  drawMyText fntTiny  112 388 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillIntimidation c) (profBonus c))
  drawCircle 102 390 3 (skillIntimidation c)
  drawMyText fntTiny  112 374 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillInvestigation c) (profBonus c))
  drawCircle 102 376 3 (skillInvestigation c)
  drawMyText fntTiny  112 360 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillMedicine c) (profBonus c))
  drawCircle 102 362 3 (skillMedicine c)
  drawMyText fntTiny  112 346 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillNature c) (profBonus c))
  drawCircle 102 348 3 (skillNature c)
  drawMyText fntTiny  112 331 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillPerception c) (profBonus c))
  drawCircle 102 333 3 (skillPerception c)
  drawMyText fntTiny  112 317 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillPerformance c) (profBonus c))
  drawCircle 102 319 3 (skillPerformance c)
  drawMyText fntTiny  112 303 (show5 $ skillBonus (charisma c) (charismaBonus c) (skillPersuasion c) (profBonus c))
  drawCircle 102 305 3 (skillPersuasion c)
  drawMyText fntTiny  112 288 (show5 $ skillBonus (intelligence c) (intelligenceBonus c) (skillReligion c) (profBonus c))
  drawCircle 102 290.5 3 (skillReligion c)
  drawMyText fntTiny  112 274 (show5 $ skillBonus (dexterity c) (dexterityBonus c) (skillSleightOfHand c) (profBonus c))
  drawCircle 102 276.5 3 (skillSleightOfHand c)
  drawMyText fntTiny  112 259 (show5 $ skillBonus (dexterity c) (dexterityBonus c) (skillStealth c) (profBonus c))
  drawCircle 102 262 3 (skillStealth c)
  drawMyText fntTiny  112 246 (show5 $ skillBonus (wisdom c) (wisdomBonus c) (skillSurvival c) (profBonus c))
  drawCircle 102 248 3 (skillSurvival c)
  drawMyText fntTiny  250 490 (show0 $ level c)
  drawMyText fntSmall  250 468 (if hitDice c == 0 then "" else "d" ++ show (hitDice c))
  drawMyText fntLarge  34 198 (if wisdom c == 0 then "" else show (10 + statBonus (wisdom c)))
  drawMyText fntSmall 225 190 (show0 $ copper (moneyPouch c))
  drawMyText fntSmall 225 163 (show0 $ silver (moneyPouch c))
  drawMyText fntSmall 225 136 (show0 $ electrum (moneyPouch c))
  drawMyText fntSmall 225 109 (show0 $ gold (moneyPouch c))
  drawMyText fntSmall 225  81 (show0 $ platinum (moneyPouch c))
  drawMyStrings fntTiny   35 165 10 (proficiencies c)
  drawMyStrings fntTiny  410 675 10 (personalityTraits c)
  drawMyStrings fntTiny  410 602 10 (ideals c)
  drawMyStrings fntTiny  410 543 10 (bonds c)
  drawMyStrings fntTiny  410 485 10 (flaws c)
  drawMyStrings fntTeeny 405 416 10 (take 38 (features c))
  drawMyStrings fntTiny 265 200 10 (equipment c)
  drawMyAttacks fntTiny 222 410 21 (attacks c)

drawPage2 :: Character -> JpegFile -> PDF()
drawPage2 c bg = setupPage bg $ do
  drawMyText fntLarge  55 751 (characterName c) 
  drawMyText fntSmall 265 767 (show0 $ age c)
  drawMyText fntSmall 265 738 (eyeColour c)
  drawMyText fntSmall 370 767 (height c)
  drawMyText fntSmall 370 738 (skinColour c)
  drawMyText fntSmall 465 767 (weight c)
  drawMyText fntSmall 465 738 (hairColour c)
  drawMyStrings fntTeeny  35 423 11.5 (backstory c)
  drawMyStrings fntTeeny 225 687 11.5 (allies c)
  drawMyStrings fntTeeny 225 433 11.5 (take 19 (drop 38 (features c)))
  drawMyStrings fntTeeny 405 433 11.5 (drop 57 (features c))
  drawMyStrings fntTeeny 225 190 11.5 (take 14 (treasure c))
  drawMyStrings fntTeeny 405 190 11.5 (drop 14 (treasure c))

drawIcePage2 :: Character -> JpegFile -> PDF()
drawIcePage2 c bg = setupPage bg $ do
  drawMyText fntLarge  55 751 (characterName c) 
  drawMyText fntSmall 265 767 (show0 $ age c)
  drawMyText fntSmall 265 738 (eyeColour c)
  drawMyText fntSmall 370 767 (height c)
  drawMyText fntSmall 370 738 (skinColour c)
  drawMyText fntSmall 465 767 (weight c)
  drawMyText fntSmall 465 738 (hairColour c)
  drawMyStrings fntTeeny  35 423 11.5 (backstory c)
  drawMyStrings fntTeeny 225 687 11.5 (allies c)
  drawMyStrings fntTeeny 225 433 11.5 (take 19 (drop 38 (features c)))
  drawMyStrings fntTeeny 405 433 11.5 (drop 57 (features c))
  drawMyStrings fntTeeny 225 190 11.5 (take 14 (treasure c))
  drawMyStrings fntTeeny 405 190 11.5 (drop 14 (treasure c))

drawSpellPage :: JpegFile -> Spellcasting -> PDF()
drawSpellPage bg c = setupPage bg $ do
  drawMyText fntLarge  70 753 (spellcastingClass c) 
  drawMyText fntLarge 278 757 (spellcastingAbility c) 
  drawMyText fntLarge 400 757 (show0 $ spellSaveDC c) 
  drawMyText fntLarge 505 757 (show0 $ spellAttackBonus c) 
  drawMyStrings fntTiny   50 643 14.7 (take 8 $ spellsCantrips c)
  drawMyStrings fntTiny  140 643 14.7 (drop 8 $ spellsCantrips c)
  drawMyStrings fntTiny   50 461 14.7 (take 13 $ spellsLevel1 c)
  drawMyStrings fntTiny  140 461 14.7 (drop 13 $ spellsLevel1 c)
  drawMyStrings fntTiny   50 224 14.7 (take 13 $ spellsLevel2 c)
  drawMyStrings fntTiny  140 224 14.7 (drop 13 $ spellsLevel2 c)
  drawMyStrings fntTiny  235 642 14.7 (take 13 $ spellsLevel3 c)
  drawMyStrings fntTiny  325 642 14.7 (drop 13 $ spellsLevel3 c)
  drawMyStrings fntTiny  235 404 14.7 (take 13 $ spellsLevel4 c)
  drawMyStrings fntTiny  325 404 14.7 (drop 13 $ spellsLevel4 c)
  drawMyStrings fntTiny  235 165 14.7 (take 9 $ spellsLevel5 c)
  drawMyStrings fntTiny  325 165 14.7 (drop 9 $ spellsLevel5 c)
  drawMyStrings fntTiny  420 642 14.7 (take 9 $ spellsLevel6 c)
  drawMyStrings fntTiny  510 642 14.7 (drop 9 $ spellsLevel6 c)
  drawMyStrings fntTiny  420 463 14.7 (take 9 $ spellsLevel7 c)
  drawMyStrings fntTiny  510 463 14.7 (drop 9 $ spellsLevel7 c)
  drawMyStrings fntTiny  420 284 14.7 (take 7 $ spellsLevel8 c)
  drawMyStrings fntTiny  510 284 14.7 (drop 7 $ spellsLevel8 c)
  drawMyStrings fntTiny  420 136 14.7 (take 7 $ spellsLevel9 c)
  drawMyStrings fntTiny  510 136 14.7 (drop 7 $ spellsLevel9 c)
  drawMyText    fntSmall  65 489    (show0 $ slotsLevel1 c)
  drawMyText    fntSmall  65 249    (show0 $ slotsLevel2 c)
  drawMyText    fntSmall 250 667    (show0 $ slotsLevel3 c)
  drawMyText    fntSmall 250 429    (show0 $ slotsLevel4 c)
  drawMyText    fntSmall 250 190    (show0 $ slotsLevel5 c)
  drawMyText    fntSmall 435 667    (show0 $ slotsLevel6 c)
  drawMyText    fntSmall 435 488    (show0 $ slotsLevel7 c)
  drawMyText    fntSmall 435 309    (show0 $ slotsLevel8 c)
  drawMyText    fntSmall 435 161    (show0 $ slotsLevel9 c)

drawIceSpellPage :: JpegFile -> Spellcasting -> PDF()
drawIceSpellPage bg c = setupPage bg $ do
  drawMyText fntLarge  70 753 (spellcastingClass c) 
  drawMyText fntLarge 278 757 (spellcastingAbility c) 
  drawMyText fntLarge 400 757 (show0 $ spellSaveDC c) 
  drawMyText fntLarge 505 757 (show0 $ spellAttackBonus c) 
  drawMyStrings fntTiny   50 643 14.7 (take 8 $ spellsCantrips c)
  drawMyStrings fntTiny  140 643 14.7 (drop 8 $ spellsCantrips c)
  drawMyStrings fntTiny   50 461 14.7 (take 13 $ spellsLevel1 c)
  drawMyStrings fntTiny  140 461 14.7 (drop 13 $ spellsLevel1 c)
  drawMyStrings fntTiny   50 224 14.7 (take 13 $ spellsLevel2 c)
  drawMyStrings fntTiny  140 224 14.7 (drop 13 $ spellsLevel2 c)
  drawMyStrings fntTiny  235 642 14.7 (take 13 $ spellsLevel3 c)
  drawMyStrings fntTiny  325 642 14.7 (drop 13 $ spellsLevel3 c)
  drawMyStrings fntTiny  235 404 14.7 (take 13 $ spellsLevel4 c)
  drawMyStrings fntTiny  325 404 14.7 (drop 13 $ spellsLevel4 c)
  drawMyStrings fntTiny  235 165 14.7 (take 9 $ spellsLevel5 c)
  drawMyStrings fntTiny  325 165 14.7 (drop 9 $ spellsLevel5 c)
  drawMyStrings fntTiny  420 642 14.7 (take 9 $ spellsLevel6 c)
  drawMyStrings fntTiny  510 642 14.7 (drop 9 $ spellsLevel6 c)
  drawMyStrings fntTiny  420 463 14.7 (take 9 $ spellsLevel7 c)
  drawMyStrings fntTiny  510 463 14.7 (drop 9 $ spellsLevel7 c)
  drawMyStrings fntTiny  420 284 14.7 (take 7 $ spellsLevel8 c)
  drawMyStrings fntTiny  510 284 14.7 (drop 7 $ spellsLevel8 c)
  drawMyStrings fntTiny  420 136 14.7 (take 7 $ spellsLevel9 c)
  drawMyStrings fntTiny  510 136 14.7 (drop 7 $ spellsLevel9 c)
  drawMyText    fntSmall  65 489    (show0 $ slotsLevel1 c)
  drawMyText    fntSmall  65 249    (show0 $ slotsLevel2 c)
  drawMyText    fntSmall 250 667    (show0 $ slotsLevel3 c)
  drawMyText    fntSmall 250 429    (show0 $ slotsLevel4 c)
  drawMyText    fntSmall 250 190    (show0 $ slotsLevel5 c)
  drawMyText    fntSmall 435 667    (show0 $ slotsLevel6 c)
  drawMyText    fntSmall 435 488    (show0 $ slotsLevel7 c)
  drawMyText    fntSmall 435 309    (show0 $ slotsLevel8 c)
  drawMyText    fntSmall 435 161    (show0 $ slotsLevel9 c)

drawBackpackPage :: JpegFile -> Character -> Backpack -> PDF()
drawBackpackPage bg c b = setupPage bg $ do
  drawMyText    fntLarge 55 751      (characterName c)
  drawMyStrings fntTiny 235 685 12.5 (bagPocket1 b)
  drawMyStrings fntTiny 235 611 12.5 (bagPocket2 b)
  drawMyStrings fntTiny 235 555.5 12.5 (bagPocket3 b)
  drawMyStrings fntTiny 235 494.5 12.5 (bagPocket4 b)
  drawMyStrings fntTiny 405 680 11.5 (bagFlapPouch b)
  drawMyStrings fntTiny 405 584 11.5 (bagMiddlePouch b)
  drawMyStrings fntTiny 232 431 11.5 (take 34 $ bagMainPouch b)
  drawMyStrings fntTiny 405 431 11.5 (drop 34 $ bagMainPouch b)
  drawMyText    fntTiny  75 329.1    (show0 $ copper $ bagCash b)
  drawMyText    fntTiny  75 306.1    (show0 $ silver $ bagCash b)
  drawMyText    fntTiny  75 283.1    (show0 $ electrum $ bagCash b)
  drawMyText    fntTiny  75 260.1    (show0 $ gold $ bagCash b)
  drawMyText    fntTiny  75 237.1    (show0 $ platinum $ bagCash b)
  drawMyStrings fntTiny  35 190.5 11.5 (gems $ bagCash b)
  drawMyText    fntTiny  75 408.7    (bagBedroll b)
  drawMyText    fntTiny  75 397.2    (bagRope b)
  drawMyText    fntTiny  75 385.7    (bagAmmo b)
  drawMyText    fntTiny  75 374.2    (bagTorches b)
  drawMyStrings fntTiny  35 189.5 11.5 (bagTreasure b)


drawBagOfHolding :: JpegFile -> Character -> BagOfHolding -> PDF()
drawBagOfHolding bg c b = setupPage bg $ do
  let rowHeight = 11.8
  drawMyText fntLarge  55 765 (characterName c)
  drawMyItems fntTiny  35 130 150 500 rowHeight (bohPGs b)
  drawMyItems fntTiny 190 338 358 674.5 rowHeight (take 48 $ bohItems b)
  drawMyItems fntTiny 383 532 553 674.5 rowHeight (drop 48 $ bohItems b)
      
drawPortableHole :: JpegFile -> Character -> PortableHole -> PDF()
drawPortableHole bg c b = setupPage bg $ do
  drawMyText fntLarge  55 750 (characterName c) 
  drawMyStrings fntTiny  45 694 11.5 (take 50 $ phItems b)
  drawMyStrings fntTiny 230 508 11.5 (take 34 $ drop 50 $ phItems b)
  drawMyStrings fntTiny 405 694 11.5 (drop 84 $ phItems b)

drawAbilities :: Character -> Maybe [[AbilityGroup]] -> PDF()
drawAbilities _ Nothing   = pure ()
drawAbilities c (Just as) = drawAbilities' c as

drawAbilities' :: Character -> [[AbilityGroup]] -> PDF()
drawAbilities' _ [] = pure ()
drawAbilities' c (a:as) = do
  page <- addPage Nothing
  drawWithPage page $ do
    drawMyText fntLarge 40 780 "Abilities"
    drawAbilityGroups 760 a
  drawAbilities' c as

drawAbilityGroups :: PDFFloat -> [AbilityGroup] -> Draw()
drawAbilityGroups _ [] = pure()
drawAbilityGroups i (a:as) = do
    i' <- drawAbilityGroup i a
    drawAbilityGroups i' as

drawAbilityGroup :: PDFFloat -> AbilityGroup -> Draw PDFFloat
drawAbilityGroup i a = do
    -- draw a horizontal line
    drawHLine (i + 12)
    drawMyText fntSmall 40 i $ groupName a
    drawMyText fntSmall 360 i $ "HD: " ++ show (groupHitDice a)
    drawMyText fntSmall 440 i $ "Count: " ++ show (groupCount a)
    drawMyStrings fntTiny 45 (i - 11.5) 11.5 (groupAbilities a)
    let h = 11.5 * fromIntegral (length (groupAbilities a)) :: PDFFloat
    return (i - h - 20)
    