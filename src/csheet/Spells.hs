{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Spells 
  ( getSpell
  , spellMap
  ) where

import DataTypes

import Data.Map (Map, fromList, lookup)
import qualified Data.Map as M

spellMap :: Map String Spell
spellMap = fromList (map (\s -> (spName s, s)) spellData)

getSpell :: String -> Maybe Spell
getSpell n = M.lookup n spellMap

spellData :: [Spell]
spellData = cantrips <> firstLevel <> secondLevel <> thirdLevel <> fourthLevel <> fifthLevel

cantrips :: [Spell]
cantrips =
  [
  -- Cantrips
    Spell 
    { spName = "Acid Splash"
    , spLevel = Cantrip
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You hurl a bubble of acid. Choose one creature within range, or choose two creatures within range that are within 5 feet of each other. A target must succeed on a Dexterity saving throw or take 1d6 acid damage."
    , spHigher = Just "This spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
  , Spell 
    { spName = "Blade Ward"
    , spLevel = Cantrip
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 round"
    , spHigher = Nothing
    , spDescription = "You extend your hand a trace a sigil of warding in the air. Until the end of your next turn, you have resistance against bludgeoning, piercing and slashing damage dealt by weapon attacks."
    }
  , Spell 
    { spName = "Booming Blade (Sword Coast Adventurers Guide)"
    , spLevel = Cantrip
    , spType = Abjuration
    , spRitual = False
    , spTime = ""
    , spRange = ""
    , spComponents = ""
    , spDuration = ""
    , spHigher = Nothing
    , spDescription = ""
    }
  , Spell
    { spName = "Chill Touch"
    , spLevel = Cantrip
    , spType = Necromancy
    , spRitual = False
    , spTime = " 1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "1 round"
    , spDescription = "You create a ghostly, skeletal hand in the space of a creature within range. Make a ranged spell attack against the creature to assail it with the chill of the grave. On a hit, the target takes 1d8 necrotic damage, and it can't regain hit points until the start of your next turn. Until then, the hand clings to the target."
    , spHigher = Just "This spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
  , Spell 
    { spName = "Control Flames"
    , spLevel = Cantrip
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You choose nonmagical flame that you can see within range and that fits within a 5-foot cube. You affect it in one of the following ways:<ul><li>You instantaneously expand the flame 5 feet in one direction, provided that wood or other fuel is present in the new location.</li><li>You instantaneously extinguish the flames within the cube.</li><li>You double or halve the area of bright light and dim light cast by the flame, change its color, or both. The change lasts for 1 hour.</li><li>You cause simple shapes - such as the vague form of a creature, an inanimate object, or a location - to appear within the flames and animate as you like. The shapes last for 1 hour.</li></ul>If you cast this spell multiple times, you can have up to three of its non-instantaneous effects active at a time, and you can dismiss such an effect as an action."
    }
  , Spell 
    { spName = "Create Bonfire"
    , spLevel = Cantrip
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spHigher = Just "The spell’s damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    , spDescription = "You create a bonfire on ground that you can see within range. Until the spell ends, the magic bonfire fills a 5-foot cube. Any creature in the bonfire’s space when you cast the spell must succeed on a Dexterity saving throw or take 1d8 fire damage. A creature must also make the saving throw when it moves into the bonfire’s space for the first time on a turn or ends its turn there.<br/>The bonfire ignites flammable objects in its area that aren’t being worn or carried."
    }
  , Spell 
    { spName = "Dancing Lights"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a bit of phosphorous or wychwood, or a glowworm)"
    , spDuration = "Concentration, up to 1 minute"
    , spHigher = Nothing
    , spDescription = "You create up to four torch-sized lights within range, making them appear as torches, lanterns or glowing orbs that hover in the air for the duration. You can also combine the four lights into one glowing vaguely humanoid form of medium size. Whichever form you choose, each light sheds dim light in a 10-foot radius.<br/>As a bonus action on your turn, you can move the lights up to 60 feet to a new spot within range. A light must be within 20 feet of another light created by this spell, and a light winks out if it exceeds the spell's range."
    }
  , Spell 
    { spName = "Encode Thoughts (Guildmasters Guide to Ravnica)"
    , spLevel = Cantrip
    , spType = Abjuration
    , spRitual = False
    , spTime = ""
    , spRange = ""
    , spComponents = ""
    , spDuration = ""
    , spHigher = Nothing
    , spDescription = ""
    }
  , Spell
    { spName = "Eldritch Blast"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "A beam of crackling energy streaks toward a creature within range. Make a ranged spell attack against the target. On a hit, the target takes 1d10 force damage.<br/>The spell creates more than one beam when you reach higher levels: two beams at 5th level, three beams at 11th level, and four beams at 17th level. You can direct the beams at the same target or at different ones. Make a separate attack roll for each beam."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Fire Bolt"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You hurl a mote of fire at a creature or object within range. Make a ranged spell attack against the target. On a hit, the target takes 1d10 fire damage. A flammable object hit by this spell ignites if it isn't being worn or carried."
    , spHigher = Just "This spell's damage increases by 1d10 when you reach 5th level (2d10), 11th level (3d10), and 17th level (4d10)."
    }
  , Spell 
    { spName = "Friends"
    , spLevel = Cantrip
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "S,M (a small amount of makeup applied to the face when this spell is cast)"
    , spDuration = "Concentration, up to 1 minute"
    , spHigher = Nothing
    , spDescription = "For the duration, you have advantage on all Charisma checks directed at one creature of your choice that isn't hostile toward you. When the spell ends, the creature realizes you used magic to influence its mood and becomes hostile toward you. A creature prone to violence might attack you. Another creature might seek retribution in other ways (at the DM's discretion), depending on the nature of your interaction with it."
    }
  , Spell 
    { spName = "Frostbite"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You cause numbing frost to form on one creature that you can see within range. The target must make a Constitution saving throw. On a failed save, the target takes 1d6 cold damage, and it has disadvantage on the next weapon attack roll it makes before the end of its next turn.<br/>The spell’s damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
  , Spell 
    { spName = "Green Flamed Blade (Sword Coast Adventurers Guide)"
    , spLevel = Cantrip
    , spType = Abjuration
    , spRitual = False
    , spTime = ""
    , spRange = ""
    , spComponents = ""
    , spDuration = ""
    , spHigher = Nothing
    , spDescription = ""
    }
  , Spell
    { spName = "Guidance"
    , spLevel = Cantrip
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one ability check of its choice. It can roll the die before or after making the ability check. The spell then ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Gust"
    , spLevel = Cantrip
    , spType = Transmutation
    , spRitual = False
    , spTime = "e:} 1 action"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You seize the air and compel it to create one of the following effects at a point you can see within range:\\begin{itemize}\\item One Medium or smaller creature that you choose must succeed on a Strength saving throw or be pushed up to 5 feet away from you.\\item You create a small blast of air capable of moving one object that is neither held nor carried and that weighs no more than 5 pounds. The object is pushed up to 10 feet away from you. It isn’t pushed with enough force to cause damage.\\item You create a harmless sensory effect using air, such as causing leaves to rustle, wind to slam shutters shut, or your clothing to ripple in a breeze.\\end{itemize}"
    , spHigher = Nothing
    }
  , Spell 
    { spName = "Infestation"
    , spLevel = Cantrip
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a living flea)"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You cause a cloud of mites, fleas, and other parasites to appear momentarily on one creature you can see within range. The target must succeed on a Constitution saving throw, or it takes 1d6 poison damage and moves 5 feet in a random direction if it can move and its speed is at least 5 feet. Roll a d4 for the direction: 1, north; 2: south; 3, east; or 4, west. This movement doesn't provoke opportunity attacks, and if the direction rolled is blocked, the target doesn't move.<br/>The spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6) and 17th level (4d6)."
    }
  , Spell 
    { spName = "Light"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,M (a firefly or phosphoresecent moss)"
    , spDuration = "1 hour"
    , spHigher = Nothing
    , spDescription = "You touch one object that is not larger than 10 feet in any dimension. Until the spell ends, the object sheds bright light in a 20-foot radius and dim light for an additional 20 feet. The light can be colored as you like. Completely covering the object with something opaque blocks the light. The spell ends if you cast it again or dismiss it as an action.<br/>If you target an object worn or held by a hostile creature, that creature must succeed on a Dexterity saving throw to avoid the spell."
    }
  , Spell 
    { spName = "Lightning Lure (Sword Coast Adventurers Guide)"
    , spLevel = Cantrip
    , spType = Abjuration
    , spRitual = False
    , spTime = ""
    , spRange = ""
    , spComponents = ""
    , spDuration = ""
    , spHigher = Nothing
    , spDescription = ""
    }
  , Spell
    { spName = "Mage Hand"
    , spLevel = Cantrip
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spDescription = "A spectral, floating hand appears at a point you choose within range. The hand lasts for the duration or until you dismiss it as an action. The hand vanishes if it is ever more than 30 feet away from you or if you cast this spell again.<br/>You can use your action to control the hand. You can use the hand to manipulate an object, open an unlocked door or container, stow or retrieve an item from an open contrainer, or pour the contents out of a vial. You can move the hand up to 30 feet each time you use it.<br/>You can't attack, activate magic items, or carry more than 10 pounds."
    , spHigher = Nothing
    }
  , Spell 
    { spName = "Mending"
    , spLevel = Cantrip
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "V,S,M (two lodestones)"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "This spell repairs a single break or tear in an object you touch, such as a broken chain link, two halves of a broken key, a torn cloak, or a leaking wineskin. As long as the break or tear is no larger than 1 foot in any dimension, you mend it, leaving no trace of former damage.<br/>This spell can physically repair a magic item or construct, but the spell can't restore magic to such an object."
    }
  , Spell 
    { spName = "Message"
    , spLevel = Cantrip
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a short piece of copper wire)"
    , spDuration = "1 round"
    , spHigher = Nothing
    , spDescription = "You point your finger toward a creature within range and whisper a message. The target (and only the target) hears the message and can reply in a whisper that only you can hear.<br/>You can cast this spell through solid objects if you are familiar with the target and know it is beyond the barrier. Magical silence, 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood blocks the spell. The spell doesn't have to follow a straight line and can freely travel around corners or through openings."
    }
  , Spell 
    { spName = "Mind Sliver (Unearthed Arcana)"
    , spLevel = Cantrip
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "1 round"
    , spHigher = Nothing
    , spDescription = "You drive a disorienting spike of psychic energy into the mind of one creature you can see within range. The target must make an Intelligence saving throw. Unless the saving throw is successful, the target takes 1d6 psychic damage, and the first time it makes a saving throw before the end of your next turn, it must roll a d4 and subtract the number rolled from the save.<br/>This spell’s damage increases by 1d6 when you reach certain levels: 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
  , Spell 
    { spName = "Minor Illusion"
    , spLevel = Cantrip
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "S,M (a bit of fleece)"
    , spDuration = "1 minute"
    , spHigher = Nothing
    , spDescription = "You create a sound or image of an object within range that lasts for the duration. The illusion also ends if you dismiss it as an action or cast this spell again.<br/>If you create a sound, its volume can range from a whisper to a scream. It can be your voice, someone else's voice, a lion's roar, a beating of drums, or any other sound you choose. The sound continues unabated throughout the duration, or you can make discrete sounds at different times before the spell ends.<br/>If you create an image of an object - such as a chair, muddy footprints or a small chest - it must be no larger than a 5-foot cube. The image can't create sound, light, smell, or any other sensory effect. Physical interaction with the image reveals it to be an illusion, because things can pass through it.<br/>If a creature uses its action to examine the sound or image, the creature can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a create discerns the illusion for what it is, the illusion becomes faint to the creature."
    }
  , Spell 
    { spName = "Mold Earth"
    , spLevel = Cantrip
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You choose a portion of dirt or stone that you can see within range and that fits within a 5-foot cube. You manipulate it in one of the following ways:\\begin{itemize}\\item If you target an area of loose earth, you can instantaneously excavate it, move it along the ground, and deposit it up to 5 feet away. This movement doesn’t have enough force to cause damage.\\item You cause shapes, colors, or both to appear on the dirt or stone, spelling out words, creating images, or shaping patterns. The changes last for 1 hour.\\item If the dirt or stone you target is on the ground, you cause it to become difficult terrain. Alternatively, you can cause the ground to become normal terrain if it is already difficult terrain. This change lasts for 1 hour.\\end{itemize}If you cast this spell multiple times, you can have no more than two of its non-instantaneous effects active at a time, and you can dismiss such an effect as an action."
    }
  , Spell 
    { spName = "Poison Spray"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "10 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You extend your hand toward a creature you can see within range and prject a puff of noxious gas from your palm. The creature must succeed on a Constitution saving throw or take 1d12 poison damage.<br/>This spell's damage increases by 1d12 when you reach 5th level (2d12), 11th level (3d12), and 17th level (4d12)."
    }
  , Spell 
    { spName = "Prestidigitation"
    , spLevel = Cantrip
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "10 feet"
    , spComponents = "V,S"
    , spDuration = "Up to 1 hour"
    , spHigher = Nothing
    , spDescription = "This spell is a minor magical trick that novice spellcasters use for practice. You create one of the following spell effects within range:\\begin{itemize}\\item You create an instantaneous, harmless sensory effect, suck as a shower of sparks, a puff of wind, faint musical notes, or an odd odour.\\item You instantaneously light or snuff out a candle, a torch, or a small campfire.\\item You chill, warm, or flavour up to 1 cubic foot of nonliving material for 1 hour.\\item You make a colour, a small mark, or a symbol appear on an object or a surface for 1 hour.\\item You create a nonmagical trinket or an illusory image that can fit in your hand and that lasts until the end of your next turn.\\end{itemize}If you cast this spell multiple times, you can have up to three of its non-instantaneous effects active at a time, and you can dismiss such an effect as an action."
    }
  , Spell 
    { spName = "Produce Flame"
    , spLevel = Cantrip
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "10 minutes"
    , spHigher = Nothing
    , spDescription = "A flickering flame appears in your hand. The flame remains there for the duration and harms neither you nor your equipment. The flame sheds bright light in a 10-foot radius and dim light for an additional 10 feet. The spell ends if you dismiss it as an action or if you cast it again.<br/>"
      <> "You can also attack with the flame, although doing so ends the spell. When you cast this spell, or as an action on a later turn, you can hurl the flame at a creature within 30 feet of you. Make a ranged spell attack. On a hit, the target takes 1d8 fire damage.<br/>"
      <> "This spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
  , Spell 
    { spName = "Ray of Frost"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "A frigid beam of blue-white light streaks toward a creature within range. Make a ranged spell attack against the target. On a hit, it takes 1d8 cold damage, and its speed is reduced by 10 feet until the start of your next turn.<br/>The spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8) and 17th level (4d8)."
    }
  , Spell 
    { spName = "Resistance"
    , spLevel = Cantrip
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a miniature cloak)"
    , spDuration = "Concentration, up to 1 minute"
    , spHigher = Nothing
    , spDescription = "You touch one willing creature. Once before the spell ends, the target can roll a d4 and add the number rolled to one saving throw of its choice. It can roll the die before or after making the saving throw. The spell then ends."
    }
  , Spell 
    { spName = "Sacred Flame"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "Flame-like radiance descends on a creature that you can see within range. The target must succeed on a Dexterity saving throw or take 1d8 radiant damage. The target gains no benefit from cover for this saving throw.<br/>The spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
  , Spell 
    { spName = "Shape Water"
    , spLevel = Cantrip
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You choose an area of water that you can see within range and that fits within a 5-foot cube. You manipulate it in one of the following ways:\\begin{itemize}\\item You instantaneously move or otherwise change the flow of the water as you direct, up to 5 feet in any direction. This movement doesn’t have enough force to cause damage.\\item You cause the water to form into simple shapes and animate at your direction. This change lasts for 1 hour.\\item You change the water’s color or opacity. The water must be changed in the same way throughout. This change lasts for 1 hour.\\item You freeze the water, provided that there are no creatures in it. The water unfreezes in 1 hour.\\end{itemize}If you cast this spell multiple times, you can have no more than two of its non-instantaneous effects active at a time, and you can dismiss such an effect as an action."
    }
  , Spell 
    { spName = "Shocking Grasp"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "Lightning springs from your hand to deliver a shock to a creature you try to touch. Make a melee spell attack against the target. You have advantage on the attack roll if the target is wearing armour made of metal. On a hit, the target takes 1d8 lightning damage, and can't take reactions until the start of its next turn.<br/>This spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
  , Spell 
    { spName = "Spare the Dying"
    , spLevel = Cantrip
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You touch a living creature that has 0 hit points. The creature becomes stable. This spell has no effect on undead or constructs."
    }
  , Spell 
    { spName = "Sword Burst (Sword Coast Adventurers Guide)"
    , spLevel = Cantrip
    , spType = Abjuration
    , spRitual = False
    , spTime = ""
    , spRange = ""
    , spComponents = ""
    , spDuration = ""
    , spHigher = Nothing
    , spDescription = ""
    }
  , Spell
    { spName = "Thunderclap"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "5 feet"
    , spComponents = "S"
    , spDuration = "Concentration, up to 1 round"
    , spDescription = "You create a burst of thunderous sound that can be heard up to 100 feet away. Each creature within range, other than you, must succeed on a Constitution saving throw or take 1d6 thunder damage."
    , spHigher = Just "The spell’s damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
  , Spell 
    { spName = "Toll the Dead"
    , spLevel = Cantrip
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You point at one creature you can see within range, and the sound of a dolorous bell fills the air around it for a moment. The target must succeed on a Wisdom saving throw or take 1d8 necrotic damage. If the target is missing any of its hit points, it instead takes 1d12 necrotic damage.<br/>The spell's damage increases by one die when you reach 5th level (2d8 or 2d12), 11th level (3d8 or 3d12) and 17th level (4d8 or 4d12)."
    }
  , Spell 
    { spName = "True Strike"
    , spLevel = Cantrip
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "S"
    , spDuration = "Concentration, up to 1 round"
    , spHigher = Nothing
    , spDescription = "You extend your hand and point a finger at a target in range. Your magic grants you a brief insight into the target's defenses. On your next turn, you gain advantage on your first attack roll against the target, provided this spell hasn't ended."
    }
  , Spell 
    { spName = "Vicious Mockery"
    , spLevel = Cantrip
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spHigher = Just "This spell's damage increases by 1d4 when you reach 5th level (2d4), 11th level (3d4) and 17th level (4d4)."
    , spDescription = "You unleash a string of insults laced with subtle enchantments at a creature you can see within range. If the target can hear you (though it need not understand you), it must succeed on a Wisdom saving throw or take 1d4 psychic damage and have disadvantage on the next attack roll it makes before the end of it's next turn."
    }
  , Spell 
    { spName = "Word of Radiance"
    , spLevel = Cantrip
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "5 feet"
    , spComponents = "V,M (a holy symbol)"
    , spDuration = "Instantaneous"
    , spHigher = Just "This spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6) and 17th level (4d6)."
    , spDescription = "You utter a divine word, and burning radiance erupts from you. Each creature of your choice that you can see within range must succeed on a Constitution saving throw or take 1d6 radiant damage."
    }
  ]

firstLevel :: [Spell]
firstLevel =
  [ Spell
    { spName = "Absorb Elements"
    , spLevel = One
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 reaction, which you take when you take acid, cold, fire, lightning or thunder damage"
    , spRange = "Self"
    , spComponents = "S"
    , spDuration = "1 round"
    , spDescription = "The spell captures some of the incoming energy, lessening its effect on you and storing it for your next melee attack. You have resistance to the triggering damage type until the start of your next turn. Also, the first time you hit with a melee attack on your next turn, the target takes an extra 1d6 damage of the triggering type, and the spell ends."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the extra damage increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Acid Stream} \normalsize\textit{(Unearthed Arcana)"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a bit of rotten food)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A stream of acid emanates from you in a line 30 feet long and 5 feet wide in the direction you choose. Each creature in the line must succeed on a Dexterity saving throw or be covered in acid for the spell's duration or until a creature uses its action to scrape or wash the acid off itself or another creature. A creature covered in the acid takes 3d4 acid damage at the start of each of its turns."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d4 for each slot level above 1st."
    }
  , Spell
    { spName = "Alarm"
    , spLevel = One
    , spType = Abjuration
    , spRitual = True
    , spTime = "1 minute"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a tiny bell and a piece of fine silver wire)"
    , spDuration = "8 hours"
    , spDescription = "You set an alarm against unwanted intrusion. Choose a door, window, or an area within range that is no larger than a 20-foot cube. Until the spell ends, an alarm alerts you whenever a Tiny or larger creature touches or enters the warded area. When you cast the spell, you can designate creatures that won't set off the alarm. You also choose whether the alarm is mental or audible.<br/>A mental alarm alerts you with a ping in your mind if you are within 1 mile of the warded area. This ping awakens you if you are sleeping.<br/>An audible alarm produces the sound of a hand bell for 10 seconds within 60 feet."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Arms of Hadar"
    , spLevel = One
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self (10 foot sphere)"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You invoke the power of Hadar, the Dark Hunger. Tendrils of dark energy erupt from you and batter all creatures within 10 feet of you. Each creature in that area must make a Strength saving throw. On a filaed save, a target takes 2d6 necrotic damage and can't take reactions until its next turn. On a successful save, the creature takes half damage, but suffers no other effect."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Burning Hands"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "15-foot cone"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "As you hold your hands with thumbs touching and fingers spread, a thin sheet of flames shoots forth from your outstretched fingertips. Each creature in a 15-foot cone must make a Dexterity saving throw. A creature takes 3d6 fire damage on a failed save, or half as much damage on a successful one.<br/>The fire ignites any flammable objects in the area that aren't being worn or carried"
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Catapult"
    , spLevel = One
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "S"
    , spDuration = "Instantaneous"
    , spDescription = "Choose one object weighing 1 to 5 pounds within range that isn't being worn or carried. The object flies in a straight line up to 90 feet in a direction you choose before falling to the ground, stopping early if it impacts against a solid surface. If the object would strike a creature, that creature must make a Dexterity saving throw. On a failed save, the object strikes the target and stops moving. When the object strikes something, the object and what it strikes each take 3d8 bludgeoning damage."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the maximum weight of objects that you can target with this spell increases by 5 pounds, and the damage increases by 1d8, for each slot level above 1st."
    }
  , Spell
    { spName = "Cause Fear"
    , spLevel = One
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You awaken the sense of mortality in one creature you can see within range. A construct or undead is immune to this effect. The target must succeed on a Wisdom saving throw or become frightened of you until the spell ends. The frightened target can repeat the saving throw at the end of each of its turns, ending the effect on itself on a success."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each slot level above 1st. The creatures must be within 30 feet of each other when you target them."
    }
  , Spell
    { spName = "Chaos Bolt"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You hurl an undulating, warbling mass of chaotic energy at one creature in range. Make a ranged spell attack against the target. On a hit, the target takes 2d8 + 1d6 damage. Choose one of the d8s. The number rolled on that die determines the attack’s damage type, as shown below.<br/>" <>
      "<table><tr><th> d8  </th><th> Damage Type </th></tr>" <>
      "<tr><td>  1  </td><td> Acid        </td></tr>" <>
      "<tr><td>  2  </td><td> Cold        </td></tr>" <>
      "<tr><td>  3  </td><td> Fire        </td></tr>" <>
      "<tr><td>  4  </td><td> Force       </td></tr>" <>
      "<tr><td>  5  </td><td> Lightning   </td></tr>" <>
      "<tr><td>  6  </td><td> Poison      </td></tr>" <>
      "<tr><td>  7  </td><td> Psychic     </td></tr>" <>
      "<tr><td>  8  </td><td> Thunder     </td></tr></table>" <>
      "If you roll the same number on both d8s, the chaotic energy leaps from the target to a different creature of your choice within 30 feet of it. Make a new attack roll against the new target, and make a new damage roll, which could cause the chaotic energy to leap again.<br/>" <>
      "A creature can be targeted only once by each casting of this spell."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, each target takes 1d6 extra damage of the type rolled for each slot level above 1st."
    }
  , Spell
    { spName = "Charm Person"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "1 hour"
    , spDescription = "You attempt to charm a humanoid you can see within range. It must make a Wisdom saving throw, and does so with advantage if you or your companions are fighting it. If it fails the saving throw, it is charmed by you until the spell ends or until you or your companions do anything harmful to it. The charmed creature regards you as a friendly acquaintance. When the spell ends, the creature knows it was charmed by you."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd-level or higher, you can target one additional creature for each slot above 1st. The creatures must be within 30 feet of each other when you terget them."
    }
  , Spell
    { spName = "Chromatic Orb"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (a diamond worth at least 50gp)"
    , spDuration = "Instantaneous"
    , spDescription = "You hurl a 4-inch-diameter sphere of energy at a creature that you can see within range. You choose acid, cold, fire, lightning, poison or thunder for the type of orb you create, and then make a ranged spell attack against the target. If the attack hits, the creature takes 3d8 damage of the type you choose."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d8 for each slot level above 1st."
    }
  , Spell
    { spName = "Colour Spray"
    , spLevel = One
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self (15-foot cone)"
    , spComponents = "V,S,M (a pinch of powder or sand that is coloured red, yellow and blue)"
    , spDuration = "1 round"
    , spDescription = "A dazzling array of flashing, coloured light springs from your hand. Roll 6d10; the total is how many hit points of creatures this spell can affect. Creatures in a 15-foot cone originating from you are affected in ascending order of their current hit points (ignoring unconscious creatures and creatures that can't see).<br/>Starting with the creature that has the lowest current hit points, each creature affected by this spell is blinded until the spell ends. Subtract each creature's hit points from the total before moving to the creature with the next lowest hit points. A creature's hit points must be equal to or less that the remaining total for that creature to be affected."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2d10 for each slot level above 1st."
    }
  , Spell
    { spName = "Command"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "1 round"
    , spDescription = "You speak a one-word command to a creture you can see within range. The target must succeed on a Wisdom saving throw or follow the command on its next turn. The spell has no effect if the target is undead, if it doesn't understand your language, or if your command is directly harmful to it.  Some typical commands and their effects follow. You might issue a command other than one described here. If you do so, the DM determines how the target behaves. If the target doesn't follow your command, the spell ends. Approach. The target moves toward you by the shortest and most direct route, ending its turn if it moves within 5 feet of you. Drop. The target drops whatever it is holding and then ends its turn.  Flee. The target spends its turn moving away from you by the fastest available means.  Grovel. The target falls prone and then ends its turn.  Halt. The target doesn't move and takes no actions. A flying creature stays aloft, provided it is able to do so. If it must move to stay aloft, it flies the minimum distance needed to remain in the air."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, you can affect one additional creature for each slot level above 1st. The creatures must be within 30 feet of each other when you target them."
    }
  , Spell
    { spName = "Comprehend Languages"
    , spLevel = One
    , spType = Divination
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a pinch of soot and salt)"
    , spDuration = "1 hour"
    , spDescription = "For the duration, you understand the literal meaning of any spoken language that you hear. You also understand any written language that you see, but you must be touching the surface on which the words are written. It takes about 1 minute to read one page of text.<br/>This spell doesn't decode secret messages in a text or a glyph, such as an arcane sigil, that isn't part of a written langauage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Create/Destroy Water"
    , spLevel = One
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a drop of water if creating water or a few grains of sand if destroying it)"
    , spDuration = "Instantaneous"
    , spDescription = "<b>Create Water.</b> You create up to 10 gallons of clean water within range in an open container. Alternatively, the water falls as rain in a 30-foot cube within range, extinguishing exposed flames in the area.<br/>" 
      <> "<b>Destroy Water.</b> You destroy up to 10 gallons of water in an open container within range. Alternatively, you destroy fog in a 30-foot cube within range."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, you create or destroy 10 additional gallons of water, or the size of the cube increases by 5 feet, for each slot level above 1st."
    }
  , Spell
    { spName = "Cure Wounds"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "A creature you touch regains a number of hit points equal to 1d8 + your spellcasting ability modifier. This spell has no effect on undead or constructs."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the healing increases by 1d8 for each slot level above 1st."
    }
  , Spell
    { spName = "Detect Magic"
    , spLevel = One
    , spType = Divination
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "For the duration, you sense the presence of magic within 30 feet of you. If you sense magic in this way, you can use your action to see a faint aura around any visible creature or object in the area that bears magic, and you learn its school of magic, if any.<br/>The spell can penetrate most barriers, but it is blocked by 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood or dirt."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Disguise Self"
    , spLevel = One
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 hour"
    , spDescription = "You make yourself - including your clothing, armour, weapons and other belongings on your person - look different until the spell ends or you use your action to dismiss it. You can seem 1 foot shorter or taller and can appear thin, fat or in between. You can't change your body type, so you must adopt a form that has the same basic arrangement of limbs. Otherwise, the extent of the illusion is up to you.<br/>The changes wrought by this spell fail to hold up to physical inspection. For example, if you use this spell to add a hat to your outfit, objects pass through the hat, and anyone who touches it would feel nothing or would feel your hear and hair. If you use this spell to appear thinner than you are, the hand of someone who reaches out to touch you would bump into you while it was seemingly still in midair.<br/>To discern that you are disguised, a creature can use its action to inspect your appearance and must succeed on an Intelligence (Investigation) check against your spell save DC."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Dissonant Whispers"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spDescription = "You whisper a discordant melody that only one creature of your choice within range can hear, wracking it with terrible pain. The target must make a Wisdom saving throw. On a failed save, it takes 3d6 psychic damage and must immediately use its reaction, if available, to move as far as possible away from you. The creature doesn't move into obviously dangerous ground, such as a fire or a pit. On a successful save, the target takes half as much damage and doesn't have to move away. A deafened creature automatically succeeds on the save."
    , spHigher = Nothing
    }
-- Distort Value (Acquisitions Incorporated)
  , Spell
    { spName = "Earth Tremor"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "10 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You cause a tremor in the ground within range. Each creature other than you in that area must make a Dexterity saving throw. On a failed save, a creature takes 1d6 bludgeoning damage and is knocked prone. If the ground in that area is loose earth or stone, it becomes difficult terrain until cleared, with each 5-foot diameter portion requiring at least 1 minute to clear by hand."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Expeditious Retreat"
    , spLevel = One
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "This spell allows you to move at an incredible pace. When you cast this spell, and then as a bonus action on each of your turns until the spell ends, you can take the Dash action."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Faerie Fire"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Each object in a 20-foot cube within range is outlined in blue, green, or violet light (your choice). Any creature in the area when the spell is cast is also outlined in light if it fails a Dexterity saving throw. For the duration, objects and affected creatures shed dim light in a 10-foot radius.<br/>"
      <> "Any attack roll against an affected creature or object has advantage if the attacker can see it, and the affected creature or object can't benefit from being invisible."
    , spHigher = Nothing
    }
  , Spell
    { spName = "False Life"
    , spLevel = One
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a small amount of alcohol or distilled spirits)"
    , spDuration = "1 hour"
    , spDescription = "Bolstering yourself with a necromantic facsimile of life, you gain 1d4+4 temporary hit points for the duration."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, you gain 5 additional temporary hit points for each slot level above 1st."
    }
  , Spell
    { spName = "Feather Fall"
    , spLevel = One
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 reaction, which you take when you or a creature within 60 feet of you falls"
    , spRange = "60 feet"
    , spComponents = "V,M (a small feather of piece of down)"
    , spDuration = "1 minute"
    , spDescription = "Choose up to 5 falling creatures within range. A falling creature's rate of descent slows to 60 feet per round until the spell ends. If the creature lands before the spell ends, it takes no falling damage and can land on its feet, and the spell ends for that creature."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Find Familiar"
    , spLevel = One
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 hour"
    , spRange = "10 feet"
    , spComponents = "V,S,M (10gp worth of charcoal, incense, and herbs that must be consumed by fire in an brass brazier)"
    , spDuration = "Instantaneous"
    , spDescription = "You gain the service of a familiar, a spirit that takes an animal form you choose: bat, cat crab, frog (toad), hawk, lizard, octopus, owl, poisonous snake, fish (quipper), rat, raven, sea horse, spider or weasel. Appearing in an unoccupied space within range, the familiar has the statistics of the chosen form, though it is a celestial, fey or fiend (your choice) instead of a beast.<br/>Your familiar acts independently of you, but it always obeys your commands. In combat, it rools its own initiative and acts on its own turn. A familiar can't attack, but it can take other actions as normal.<br/>When the familiar drops to 0 hit points, it disappears, leaving behind no physical form. It reappears after you cast this spell again.<br/>While your familiar is within 100 feet of you, you can communicate with it telepathically. Additionally, as an action, you can see through your familiar's eyes and hear what it hears until the start of your next turn, gaining the benefits of any special senses that the familiar has. During this time, you are deaf and blind with regard to your own senses.<br/>As an action, you can temprarily dismiss your familiar. It disappears into a pocket dimension where it awaits your summons. Alternatively, you can dismiss it forever. As an action while it is temporarily dismissed, you can cause it to appear in any unoccupied space within 30 feet of you.<br/>You can't have more than one familiar at a time. If you cast this spell while you already have a familiar, you instead cause it to adopt a new form. Choose one of the forms from the above list. Your familiar transforms into the chosen creature.<br/>Finally, when you cast a spell with a range of touch, your familiar can deliver the spell as if it had cast the spell. Your familiar must be within 100 feet of you, and it must use its reaction to deliver the spell when you cast it. If the spell requires an attack roll, you use your attack modifier for the roll."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Fog Cloud"
    , spLevel = One
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You create a 20-foot radius sphere of fog centered on a point within range. The sphere spreads around corners, and its area is heavily obscured. It lasts for the duration or until a wind of moderate or greater speed (at least 10 miles per hour) disperses it."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the radius of the fog increases by 20 feet for each slot level above 1st."
    }
-- Frost Fingers (Icewind Dale: Rime of the Frostmaiden)
  , Spell
    { spName = "Grease"
    , spLevel = One
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of pork rind or butter)"
    , spDuration = "1 minute"
    , spDescription = "Slick grease covers the ground in a 10-foot square centered on a point within range and turns it into difficult terrain for the duration.<br/>When the grease appears, each creature standing in its area must succeed on a Dexterity saving throw or fall prone. A creature that enters the area or ends its turn there must also succeed on a Dexterity saving throw or fall prone."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Guiding Bolt"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "1 round"
    , spDescription = "A flash of light streaks toward a creature of your choice within range. Make a ranged spell attack against the target. On a hit, the target takes 4d6 radiant damage, and the next attack roll made against this target before the end of your next turn has advantage, thanks to the mystical dim light glittering on the target until then."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Healing Word"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spDescription = "A creature of your choice that you can see within range regains hit points equal to 1d4 + your spellcasting ability modifier. This spell has no effect on undad or constructs."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the healing increases by 1d4 for each slot level above 1st."
    }
  , Spell
    { spName = "Heroism"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A willing creature you touch is imbued with bravery. Until the spell ends, the creature is immune to being frightened and gains temprary hit points equal to your spellcasting modifier at the start of each of its turns. When the spell ends, the target loses any remaining temporary hit points from this spell."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each spell slot above first."
    }
  , Spell
    { spName = "Hex"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Touch"
    , spComponents = "V,S,M (the petrified eye of a newt)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You place a curse on a creature that you can see within range. Until the spell ends, you deal an extra 1d6 necrotic damage to the target whenever you hit it with an attack. Also, choose one ability when you cast the spell. The target has disadvantage on ability checks made with the chosen ability.<br/>If the target drops to 0 hit points before this spell ends, you can use a bonus action on a subsequent turn of yours to curse a new creature.<br/>A Remove Curse cast on the target ends this spell early."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd or 4th level, you can maintain your concentration on the spell for up to 8 hours. When you use a spell slot of 5th level or higher, you can maintain your concentration on the spell for up to 24 hours."
    }
  , Spell
    { spName = "Ice Knife"
    , spLevel = One
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "S,M (a drop of water or a piece of ice)"
    , spDuration = "Instantaneous"
    , spDescription = "You create a shard of ice and fling it at one creature within range. Make a ranged spell attack against the target. On a hit, the target takes 1d10 piercing damage. Hit or miss, the shard then explodes. The target and each creature within 5 feet of it must succeed on a Dexterity saving throw or take 2d6 cold damage."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the cold damage increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Identify"
    , spLevel = One
    , spType = Divination
    , spRitual = True
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "V,S,M (a pearl worth 100gp and an owl feather)"
    , spDuration = "Instantaneous"
    , spDescription = "You choose one object that you must touch throughout the casting of the spell. If it is a magic item or some other magic-imbued object, you learn its properties and how to use them, whether it requires attunement to use, and how many charges it has, if any. You learn whether any spells are affecting the item and what they are. If the item was created by a spell, you learn which spell created it.<br/>If you instead touch a creature throughout the casting, you learn what spells, if any, are currently affecting it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Illusory Script"
    , spLevel = One
    , spType = Illusion
    , spRitual = True
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "S,M (a lead-based ink worth at least 10gp which the spell consumes)"
    , spDuration = "10 days"
    , spDescription = "You write on parchment, paper or some other suitable writing material and imbue it with a potent illusion that lasts for the duration.<br/>To you and any creatures you designate when you cast the spell, the writing appears normal, written in your hand, and conveys whatever meaning you intended when you wrote the text. To all others, the writing appears as if it were in an unknown or magical script that is unintelligible. Alternatively, you can cause the writing to appear to be an entirely different message, written in a different hand and language, thought the language must be one that you know.<br/>Should the spell be dispelled, the original script and the illusion both disappear.<br/>A creature with truesight can read the hidden message."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Jump"
    , spLevel = One
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a grasshopper's hind leg)"
    , spDuration = "1 minute"
    , spDescription = "You touch a creature. The creatur's jump distance is tripled until the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Longstrider"
    , spLevel = One
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a pinch of dirt)"
    , spDuration = "1 hour"
    , spDescription = "You touch a creture. The target's speed increases by 10 feet until the spell ends."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, you can target one additional creature for each slot level above 1st."
    }
  , Spell
    { spName = "Mage Armor"
    , spLevel = One
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a piece of cured leather)"
    , spDuration = "8 hours"
    , spDescription = "You touch a willing creature who isn't wearing armour, and a protective magical force surrounds it until the spell ends. The target's base AC becomes 13 + its Dexterity modifier. The spell ends if the target dons armour or if you dismiss the spell as an action."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Magic Missile"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You create three glowing darts of magical force. Each dart hist a creature of your choice that you can see within range. A dart deals ad4+1 force damage to its target. The darts all strike simultaneously, and you can direct them to hit ont target or several."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the spell creates one more dart for each slot level above 1st."
    }
  , Spell
    { spName = "Protection from Evil and Good"
    , spLevel = One
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (holy water or powdered silver and iron, which the spell consumes)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "Until the spell ends, one willing creature you touch is protected against certain types of creatures: abberations, celestials, elementals, fey, fiends and undead.<br/>The protection grants several benefits. Creatures of those types have disadvantage on attack rools against the target. The target also can't be charmed, frightened, or possessed by them. If the target is already charmed, frightened, or possessed by such a creature, the target has advantage on any new saving throw against the relevant effect."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Purify Food and Drink"
    , spLevel = One
    , spType = Transmutation
    , spRitual = True
    , spTime = "1 action"
    , spRange = "10 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "All nonmagical food and drink within a 5-foot-radius sphere centered on a point of your choice within range is purified and rendered free of poison and disease."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Ray of Sickness"
    , spLevel = One
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "A ray of sickening greenish energy lashes out toward a creature within range. Make a ranged spell attack against the target. On a hit, the target takes 2d8 poison damage and must make a Constitution saving throw. On a failed save, it is also poisoned until the end of your next turn."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d8 for each slot level above 1st."
    }
  , Spell
    { spName = "Sanctuary"
    , spLevel = One
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a small silver mirror)"
    , spDuration = "1 minute"
    , spDescription = "You ward a creature within range against attack. Until the spell ends, any creature who targets the warded creature with an attack or a harmful spell must first make a Wisdom saving throw. On a failed save, the creature must choose a new target or lose the attack or spell. This spell doesn't protect the warded creature from area effects, such as the explosion of a fireball.<br/>"
      <> "If the warded creature makes an attack, casts a spell that affects an enemy, or deals damage to another creature, this spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Searing Smite"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Self"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "The next time you hit a creature with a melee weapon attack during the spell's duration, your weapon flares with white-hot intensity, and the attack deals an extra 1d6 fire damage to the target and causes the target to ignite in flames. At the start of each of its turns until the spell ends, the target must make a Constitution saving throw. On a failed save, it takes 1d6 fire damage. On a successful save, the spell ends. If the target or a creature within 5 feet of it uses an action to put out the flames, or if some other effect douses the flames (such as the target being submerged in water), the spell ends."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the initial extra damage dealt by the attack increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Shield"
    , spLevel = One
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 reaction, which you take when you are hit by an attack or targeted by the \textit{magic missile} spell"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 round"
    , spDescription = "An invisible barrier of magical force appears and protects you. Until the start of your next turn, you have a +5 bonus to AC, including against the triggering attack, and you take no damage from \\textit{magic missile}."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Shield of Faith"
    , spLevel = One
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a small parchment with a bit of holy text written on it)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "A shimmering field appears and surrounds a creature of your choice within range, granting it a +2 bonus to AC for the duration."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Silent Image"
    , spLevel = One
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of fleece)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "You create the image of an object, a creature, or some other visible phenomenon that is no larger than a 15-foot cube. The image appears at a spot within range and lasts for the duration. The image is purely visual; it isn't accompanied by sounds, smell, or other sensory effects.<br/>You can use your action to cause the image to move to any spot within range. As the image changes location, you can alter its appearance so that its movements appear natural for the image. For example, if you create an image of a creature and move it, you can alter the image so that it appears to be walking.<br/>Physical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Silvery Barbs"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 reaction, which you take when a creature you can see within 60 feet of yourself succeeds on an attack roll, an ability check, or a saving throw"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spDescription = "You magically distract the triggering creature and turn its momentary uncertainty into encouragement for another creature. The triggering creature must reroll the d20 and use the lower roll.<br/>"
      <> "You can then choose a different creature you can see within range (you can choose yourself). The chosen creature has advantage on the next attack roll, ability check, or saving throw it makes within 1 minute. A creature can be empowered by only one use of this spell at a time."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Sleep"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (a pinch of fine sand, rose petals, or a cricket)"
    , spDuration = "1 minute"
    , spDescription = "This spell sends creatures into a magical slumber. Roll 5d8: the total is how many hit points of creatures this spell can affect. Creatures within 20 feet of a point you choose within range are affected in ascending order of their current hit points (ignoring unconscious creatures).<br/>Starting with the creature that has the lowest current hit points, each creature affected by this spell falls unconscious until the spell ends, the sleeper takes damage, or someone uses an action to shake or slap the sleeper awake. Subtract each creatures's hit points from the total before moving on to the creature with the next lowest hit points. A creature's hit points must be equal to or less than the remaining total for that creature to be affected.<br/>Undead and creatures immune to being charmed aren't affected by this spell."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2d8 for each slot level above 1st."
    }
  , Spell
    { spName = "Snare"
    , spLevel = One
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "S,M (25 feet of rope, which the spell consumes)"
    , spDuration = "8 hours"
    , spDescription = "As you cast this spell, you use the rope to create a circle with a 5-foot radius on the ground or the floor. When you finish casting, the rope disappears and the circle becomes a magic trap.<br/>This trap is nearly invisible, requiring a successful Intelligence (Investigation) check against your spell save DC to be discerned.<br/>The trap triggers when a Small, Medium, or Large creature moves onto the ground or the floor in the spell's radius. That creature must succeed on a Dexterity saving throw or be magically hoisted into the air, leaving it hanging upside down 3 feet above the ground or floor. The creature is restrained there until the spell ends.<br/>A restrained creature can make a Dexterity saving throw at the end of each of its turns, ending the effect on iteself on a success. Alternatively, the creature or someone else who can reach it can use an action to make an Intelligence (Arcana) check against your spell save DC. On a success, the restrained effect ends.<br/>After the trap is triggered, the spell ends when no creature is restrained by it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Tashas Caustic Brew"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a bit of rotten food)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A stream of acid emanates from you in a line 30 feet long and 5 feet wide in a direction you choose. Each creature in the line must succeed on a Dexterity saving throw or be covered in acid for the spells duration or until a creature uses its action to scrape or wash the acid off itself or another creature. A creature covered in the acid takes 2d4 acid damage at start of each of its turns."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 2d4 for each slot level above 1st."
    }
  , Spell
    { spName = "Tashas Hideous Laughter"
    , spLevel = One
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (tiny tarts and a feather that is waved in the air)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A creature of your choice that you can see within range perceives everything as hilariously funny and falls into fits of laughter if this spell affects it. The target must succeed on a Wisdom saving throw or fall prone, becoming incapacitated and unable to stand up for the duration. A creature with an intelligence score of 4 or less isn't affected.<br/>At the end of each of its turns, and each time it takes damage, the target can make another Wisdom saving throw. The target has advantage on the saving throw if it's triggered by damage. On success, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Tenser's Floating Disk"
    , spLevel = One
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a drop of mercury)"
    , spDuration = "1 hour"
    , spDescription = "This spell creates a circular, horizontal plane of force, 3 feet in diameter and 1 inch thick, that floats 3 feet above groud in an unoccupiied space of your choice that you can see within range. The disk remains for the duration, and can hold up to 500 pounds. If more weight is placed on it, the spell ends, and everything on the disk falls to the ground.<br/>The disk is immobile while you are within 20 feet of it. If you move more than 20 feet away from it, the disk follows you so that it remains within 20 feet of you. It can move across uneven terrain, up or down stairs, slopes and the like, but it can't cross an elevation change of 10 feet or more. For example, the disk can't move across a 10-foot-deep pit, nor could it leave such a pit if it was created at the bottom.<br/>If you move more than 100 feet from the disk (typically because it can't move around an obstacle to follow you), the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Thunderwave"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self (15-foot cube)"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "A wave of thunderous force sweeps out from you. Each creature in a 15-foot cube originating from you must make a Constitution saving throw. On a failed save, a creature takes 2d8 thunder damage and is pushed 10 feet away from you. On a successful save, the creature takes half as much damage and isn't pushed.<br/>In addition, unsecured objects that are completely within the area of effect are automatically pushed 10 feet away from you by the spell's effect, and the spell emits a thunderous boom audible out to 300 feet."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d8 for each slot level above 1st."
    }
  , Spell
    { spName = "Unseen Servant"
    , spLevel = One
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a piece of string and a bit of wood)"
    , spDuration = "1 hour"
    , spDescription = "This spell creates an invisible, mindless, shapeless force that performs simple tasks at your command until the spell ends. The servant springs into existence in an unoccupied space on the ground within range. It has AC 10, 1 hit point, and a Strength of 2, and it can't attack. If it drops to 0 hit points, the spell ends.<br/>Once on each of your turns as a bonus action, you can mentally command the servant to move up to 15 feet and interact with an object. The servant can perform simple tasks that a human servant would do, such as fetching things, cleaning, mending, folding clothes, lighting fires, serving food, and pouring wine. Once you give the command, the servant performs the task to the best of its ability until it completes the task, then waits for your next command.<br/>If you command the servant to perform a task that would move it more than 60 feet away from you, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Witch Bolt"
    , spLevel = One
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a twig from a tree that has been struck by lightning)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A beam of crackling, blue energy lances out toward a creature within range, forming a sustained arc of lightning between you and the target. Make a ranged spell attack against that creature. On a hit, the target takes 1d12 lightning damage, and on each of your turns for the duration, you can use your action to deal 1d12 lightning damage to the target automatically. The spell ends if you use your action to do anything else. The spell also ends if the target is ever outside the spell's range or if it has total cover from you."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the initial damage increases by 1d12 for each slot level above 1st."
    }
  ]

secondLevel :: [Spell]
secondLevel =
  [ Spell
    { spName = "Acid Arrow"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (powdered rhubarb and an adder's stomach)"
    , spDuration = "Instantaneous"
    , spDescription = "A shimmering green arrow streaks toward a target within range and bursts in a spray of acid. Make a ranged spell attack against the target. On a hit, the target takes 4d4 acid damage immediately and 2d4 acid damage at the end of its next turn. On a miss, the arrow splashes the target with acid for half as much of the initial damage and no damage at the end of its next turn."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage (both initial and later) increases by 1d4 for each slot level above 2nd."
    }
  , Spell
    { spName = "Agnazzar's Scorcher"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a Red Dragon's Scale)"
    , spDuration = "Instantaneous"
    , spDescription = "A line of roaring flame 30 feet long and 5 feet wide emanates from you in a direction you choose. Each creature in the line must make a Dexterity saving throw. A creature takes 3d8 fire damage on a failed save, or half as much damage on a successful one."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
  , Spell
    { spName = "Alter Self"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 hour (concentration)"
    , spDescription = "You assume a different form. When you cast the spell, choose one of the following options, the effects of which last for the duration of the spell. While the spell lasts, you can end one option as an action to gain the benefits of a different one.<br/><b><i>Aquatic Adaptation.</i></b> You adapt your body to an aquatic environment, sprouting gills and growing webbing between your fingers. You can breathe underwater and gain a swimming speed equal to your walking speed.<br/><b><i>Change Appearance.</i></b> You transform your appearance. You decide what you look like, including your height, weight, facial features, sound of your voice, hair length, coloration, and distinguishing characteristics, if any. You can make yourself appear as a member of another race, though none of your statistics change. You also can't appear as a creature of a different size than you, and your basic shape stays the same; if you're bipedal, you can't use this spell to become quadrupedal, for instance. At any time for the duration of the spell, you can use your action to change your appearance in this way again.<br/><b><i>Natural Weapons.</i></b> You grow claws, fangs, spines, horns, or a different natural weapon of your choice. Your unarmed strikes deal 1d6 bludgeoning, piercing, or slashing damage, as appropriate to the natural weapon you chose, and you are proficient with your unarmed strikes. Finally, the natural weapon is magic and you have a +1 bonus to the attack and damage rolls you make using it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Aid"
    , spLevel = Two
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a tiny strip of white cloth)"
    , spDuration = "8 hours"
    , spDescription = "Your spell bolsters your allies with toughness and resolve. Choose up to three creatures within range. Each target's hit point maximum and current hit points increase by 5 for the duration."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, a target's hit points increase by an additional 5 for each slot level above 2nd."
    }
  , Spell
    { spName = "Arcane Lock"
    , spLevel = Two
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (gold dust worth at least 25gp, which the spell consumes)"
    , spDuration = "Until dispelled"
    , spDescription = "You touch a closed door, window, gate, chest, or other entryway, and it becomes locked for the duration. You and the creatures you designate when you cast this spell can open the object normally. You can also set a password that, when spoken within 5 feet of the object, suppresses this spell for 1 minute. Otherwise, it is impassable until it is broken or the spell is dispelled or suppressed. Casting <i>knock</i> on the object suppresses <i>arcane lock</i> for 10 minutes. <br/>"
      <> "While affected by this spell, the object is more difficult to break or force open; the DC to break it or pick any locks on it increases by 10."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Arcanists Magic Aura"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a small square of silk)"
    , spDuration = "24 Hours"
    , spDescription = "You place an illusion on a creature or an object you touch so that divination spells reveal false information about it. The target can be a willing creature or an object that isn't being carried or worn by another creature.<br/>When you cast the spell, choose one or both of the following effects. The effect lasts for the duration. If you cast this spell on the same creature or object every day for 30 days, placing the same effect on it each time, the illusion lasts until it is dispelled.<br/>"
      <> "<b>False Aura.</b> You change the way the target appears to spells and magical effects, such as <i>detect magic</i>, that detect magical auras. You can make a nonmagical object appear magical, a magical object appear nonmagical, or change the object's magical aura so that it appears to belong to a specific school of magic that you choose. When you use this effect on an object, you can make the false magic apparent to any creature that handles the item.<br/>"
      <> "<b>Mask.</b> You change the way the target appears to spells and magical effects that detect creature types, such as a paladin's Divine Sense or the trigger of a <i>symbol</i> spell. You choose a creature type and other spells and magical effects treat the target as if it were a creature of that type or of that alignment."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Blindness/Deafness"
    , spLevel = Two
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V"
    , spDuration = "1 Minute"
    , spDescription = "You can blind or deafen a foe. Choose one creature that you can see within range to make a Constitution saving throw. If it fails, the target is either <i>blinded</i> or <i>deafened</i> (your choice) for the duration. At the end of each of its turns, the target can make a Constitution saving throw. On a success, the spell ends."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd."
    }
  , Spell
    { spName = "Blur"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 Minute"
    , spDescription = "Your body becomes blurred, shifting and wavering to all who can see you. For the duration, any creature has disadvantage on attack rolls against you. An attacker is immune to this effect if it doesn't rely on sight, as with <i>blindsight</i>, or can see through illusions, as with <i>truesight</i>."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Cloud of Daggers"
    , spLevel = Two
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a sliver of glass)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You fill the air with spinning daggers in a cube 5 feet on each side, centered on a point you choose within range. A creature takes 4d4 slashing damage when it enter's the spells area for the first time on a turn or starts it's turn there."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 2d4 for each slot level above 2nd."
    }
  , Spell
    { spName = "Continual Flame"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (ruby dust worth 50gp, which the spell consumes)"
    , spDuration = "Until dispelled"
    , spDescription = "A flame, equivalent in brightness to a torch, springs forth from an object that you touch. The effect looks like a regular flame, but it creates no heat and doesn't use oxygen. A <i>continual flame</i> can be covered or hidden but not smothered or quenched."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Crown of Madness"
    , spLevel = Two
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "One humanoid of your choice that you can see within range must succeed on a Wisdom saving throw or become charmed by you for the duration. While the target is charmed in this way, a twisted crown of jagged iron appears on its head, and madness glows in its eyes.<br/>The charmed target must use its action before moving on each of its turns to make a melee attack against a creature other than itself that you mentally choose. The target can act normally on its turn if you choose no creature or if none are within reach.<br/>On your subsequent turns, you must use your action to maintain control over the target, or the spell ends. Also, the target can make a Wisdom saving throw at the end of each of its turns. An a success, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Darkness"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,M (bat fur and a drop of pitch or piece of coal)"
    , spDuration = "10 minutes"
    , spDescription = "Magical darkness spreads from a point you choose within range to fill a 15-foot-radius sphere for the duration. The darkness spreads around corners. A creature with <i>darkvision</i> can't see through this darkness, and nonmagical light can't illuminate it.<br/>If the point you choose is on an object you are holding or one that isn't being worn or carried, the darkness emanates from the object and moves with it. Completely covering the source of the darkness with an opaque object, such as a bowl or a helm, blocks the darkness.<br/>If any of this spell's area overlaps with an area of light created by a spell of 2nd level or lower, the spell that created the light is dispelled."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Darkvision"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (either a pinch of dried carrot or an agate)"
    , spDuration = "8 hours"
    , spDescription = "You touch a willing creature to grant it the ability to see in the dark. For the duration, that creature has <i>darkvision</i> out to a range of 60 feet."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Detect Thoughts"
    , spLevel = Two
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a copper piece)"
    , spDuration = "1 minute"
    , spDescription = "For the duration, you can read the thoughts of certain creatures. When you cast the spell and as your action on each turn until the spell ends, you can focus your mind on any one creature that you can see within 30 feet of you. If the creature you choose has an Intelligence of 3 or lower or doesn't speak any language, the creature is unaffected.<br/>You initially learn the surface thoughts of the creature--what is most on its mind in that moment. As an action, you can either shift your attention to another creature's thoughts or attempt to probe deeper into the same creature's mind. If you probe deeper, the target must make a Wisdom saving throw. If it fails, you gain insight into its reasoning (if any), its emotional state, and something that looms large in its mind (such as something it worries over, loves, or hates). If it succeeds, the spell ends. Either way, the target knows that you are probing into its mind, and unless you shift your attention to another creature's thoughts, the creature can use its action on its turn to make an Intelligence check contested by your Intelligence check; if it succeeds, the spell ends.<br/>Questions verbally directed at the target creature naturally shape the course of its thoughts, so this spell is particularly effective as part of an interrogation.<br/>You can also use this spell to detect the presence of thinking creatures you can't see. When you cast the spell or as your action during the duration, you can search for thoughts within 30 feet of you. The spell can penetrate barriers, but 2 feet of rock, 2 inches of any metal other than lead, or a thin sheet of lead blocks you. You can't detect a creature with an Intelligence of 3 or lower or one that doesn't speak any language.<br/>Once you detect the presence of a creature in this way, you can read its thoughts for the rest of the duration as described above, even if you can't see it, but it must still be within range."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Dragon's Breath"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a hot pepper)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You touch one willing creature and imbue it with the power to spew magical energy from its mouth, provided it has one. Choose acid, cold, fire, lightning, or poison. Until the spell ends, the creature can use an action to exhale energy of the chosen type in a 15-foot cone. Each creature in the area must make a Dexterity saving throw, taking 3d6 damage of the chosen type on a failed save, or half as much damage on a successful one."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d6 for each slot level above 2nd."
    }
  , Spell
    { spName = "Dust Devil"
    , spLevel = Two
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a pinch of dust)"
    , spDuration = "1 minute"
    , spDescription = "Choose an unoccupied 5-foot cube of air that you can see within range. An elemental force that resembles a dust devil appears in the cube and lasts for the spell’s duration.<br/>Any creature that ends its turn within 5 feet of the dust devil must make a Strength saving throw. On a failed save, the creature takes 1d8 bludgeoning damage and is pushed 10 feet away. On a successful save, the creature takes half as much damage and isn’t pushed.<br/>As a bonus action, you can move the dust devil up to 30 feet in any direction. If the dust devil moves over sand, dust, loose dirt, or small gravel, it sucks up the material and forms a 10-foot-radius cloud of debris around itself that lasts until the start of your next turn. The cloud heavily obscures its area."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
  , Spell
    { spName = "Earthbind"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "300 feet"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose one creature you can see within range. Yellow strips of magical energy loop around the creature. The target must succeed on a Strength saving throw, or its flying speed (if any) is reduced to 0 feet for the spell’s duration. An airborne creature affected by this spell safely descends at 60 feet per round until it reaches the ground or the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Enhance Ability"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (fur or feather from a beast)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You touch a creature and bestow upon it a magical enhancement. Choose one of the following effects; the target gains that effect until the spell ends.<br/>"
      <> "<b><i>Bear's Endurance.</i></b> The target has advantage on Constitution checks. It also gains 2d6 temporary hit points, which are lost when the spell ends.<br/>"
      <> "<b><i>Bull's Strength.</i></b> The target has advantage on Strength checks, and his or her carrying capacity doubles.<br/>"
      <> "<b><i>Cat's Grace.</i></b> The target has advantage on Dexterity checks. It also doesn't take damage from falling 20 feet or less if it isn't incapacitated.<br/>"
      <> "<b><i>Eagle's Splendor.</i></b> The target has advantage on Charisma checks.<br/>"
      <> "<b><i>Fox's Cunning.</i></b> The target has advantage on Intelligence checks.<br/>"
      <> "<b><i>Owl's Wisdom.</i></b> The target has advantage on Wisdom checks"
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd."
    }
  , Spell
    { spName = "Enlarge/Reduce"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a pinch of powdered iron)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You cause a creature or an object you can see within range to grow larger or smaller for the duration. Choose either a creature or an object that is neither worn nor carried. If the target is unwilling, it can make a Constitution saving throw. On a success, the spell has no effect.<br/>If the target is a creature, everything it is wearing and carrying changes size with it. Any item dropped by an affected creature returns to normal size at once.<br/>"
      <> "<b>Enlarge.</b> The target's size doubles in all dimensions, and its weight is multiplied by eight. This growth increases its size by one category - from Medium to Large, for example. If there isn't enough room for the target to double its size, the creature or object attains the maximum possible size in the space available. Until the spell ends, the target also has advantage on Strength checks and Strength saving throws. The target's weapons also grow to match its new size. While these weapons are enlarged, the target's attacks with them deal 1d4 extra damage.<br/>"
      <> "<b>Reduce.</b> The target's size is halved in all dimensions, and its weight is reduced to one-eighth of normal. This reduction decreases its size by one category - from Medium to Small, for example. Until the spell ends, the target also has disadvantage on Strength checks and Strength saving throws. The target's weapons also shrink to match its new size. While these weapons are reduced, the target's attacks with them deal 1d4 less damage (this can't reduce the damage below 1)."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Flaming Sphere"
    , spLevel = Two
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of tallow, a pinch of brimstone, and a dusting of powdered iron)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A 5-foot-diameter sphere of fire appears in an unoccupied space of your choice within range and lasts for the duration. Any creature that ends its turn within 5 feet of the sphere must make a Dexterity saving throw. The creature takes 2d6 fire damage on a failed save, or half as much damage on a successful one.<br/>As a bonus action, you can move the sphere up to 30 feet. If you ram the sphere into a creature, that creature must make the saving throw against the sphere's damage, and the sphere stops moving this turn.<br/>When you move the sphere, you can direct it over barriers up to 5 feet tall and jump it across pits up to 10 feet wide. The sphere ignites flammable objects not being worn or carried, and it sheds bright light in a 20-foot radius and dim light for an additional 20 feet."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d6 for each slot level above 2nd."
    }
  , Spell
    { spName = "Flock of Familiars (Lost Laboratory of Kwalish)"
    , spLevel = Two
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "V,S,M ()"
    , spDuration = "1 Hour"
    , spDescription = ""
    , spHigher = Nothing
    }
  , Spell
    { spName = "Gentle Repose"
    , spLevel = Two
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a pinch of salt and one copper piece placed on each of the corpse's eyes, which must remain there for the duration)"
    , spDuration = "10 days"
    , spDescription = "You touch a corpse or other remains. For the duration, the target is protected from decay and can't become undead.<br/>The spell also effectively extends the time limit on raising the target from the dead, since days spent under the influence of this spell don't count against the time limit of spells such as <i>raise dead</i>."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Gift of Gab (Acquisitions Incorporated)"
    , spLevel = Two
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 reaction*"
    , spRange = "Self"
    , spComponents = ""
    , spDuration = "Instantaneous"
    , spDescription = ""
    , spHigher = Nothing
    }
  , Spell
    { spName = "Gust of Wind"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a legume seed)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A line of strong wind 60 feet long and 10 feet wide blasts from you in a direction you choose for the spell's duration. Each creature that starts its turn in the line must succeed on a Strength saving throw or be pushed 15 feet away from you in a direction following the line.<br/>Any creature in the line must spend 2 feet of movement for every 1 foot it moves when moving closer to you.<br/>The gust disperses gas or vapor, and it extinguishes candles, torches, and similar unprotected flames in the area. It causes protected flames, such as those of lanterns, to dance wildly and has a 50 percent chance to extinguish them.<br/>As a bonus action on each of your turns before the spell ends, you can change the direction in which the line blasts from you."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Heat Metal"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a piece of iron and a flame)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose a manufactured metal object, such as a metal weapon or a suit of heavy or medium metal armor, that you can see within range. You cause the object to glow red-hot. Any creature in physical contact with the object takes 2d8 fire damage when you cast the spell. Until the spell ends, you can use a bonus action on each of your subsequent turns to cause this damage again.<br/>"
      <> "If a creature is holding or wearing the object and takes the damage from it, the creature must succeed on a Constitution saving throw or drop the object if it can. If it doesn't drop the object, it has disadvantage on attack rolls and ability checks until the start of your next turn."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
  , Spell
    { spName = "Hold Person"
    , spLevel = Two
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a small. straight piece of iron)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose a humanoid that you can see within range. The target must succeed on a Wisdom saving throw or be <i>paralyzed</i> for the duration. At the end of each of its turns, the target can make another Wisdom saving throw. On a success, the spell ends on the target."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional humanoid for each slot level above 2nd. The humanoids must be within 30 feet of each other when you target them."
    }
  , Spell
    { spName = "Invisibility"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (an eyelash encased in gum arabic)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "A creature you touch becomes <i>invisible</i> until the spell ends. Anything the target is wearing or carrying is <i>invisible</i> as long as it is on the target's person. The spell ends for a target that attacks or casts a spell."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd."
    }
  , Spell
    { spName = "Jim's Glowing Coin (Acquisitions Incorporated)"
    , spLevel = Two
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = ""
    , spDuration = "1 minute"
    , spDescription = ""
    , spHigher = Nothing
    }
  , Spell
    { spName = "Knock"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spDescription = "Choose an object that you can see within range. The object can be a door, a box, a chest, a set of manacles, a padlock, or another object that contains a mundane or magical means that prevents access.<br/>A target that is held shut by a mundane lock or that is stuck or barred becomes unlocked, unstuck, or unbarred. If the object has multiple locks, only one of them is unlocked.<br/>If you choose a target that is held shut with <i>arcane lock</i>, that spell is suppressed for 10 minutes, during which time the target can be opened and shut normally.<br/>When you cast the spell, a loud knock, audible from as far away as 300 feet, emanates from the target object."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Lesser Restoration"
    , spLevel = Two
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You touch a creature and can end either one disease or one condition afflicting it. The condition can be blinded, deafened, paralyzed, or poisoned."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Levitate"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (either a small leather loop or a piece of golden wire bent into a cup shape with a long shank on one end)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "One creature or loose object of your choice that you can see within range rises vertically, up to 20 feet, and remains suspended there for the duration. The spell can levitate a target that weighs up to 500 pounds. An unwilling creature that succeeds on a Constitution saving throw is unaffected.<br/>The target can move only by pushing or pulling against a fixed object or surface within reach (such as a wall or a ceiling), which allows it to move as if it were climbing. You can change the target's altitude by up to 20 feet in either direction on your turn. If you are the target, you can move up or down as part of your move. Otherwise, you can use your action to move the target, which must remain within the spell's range.<br/>When the spell ends, the target floats gently to the ground if it is still aloft."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Locate Object"
    , spLevel = Two
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a forked twig)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "Describe or name an object that is familiar to you. You sense the direction to the object's location, as long as that object is within 1,000 feet of you. If the object is in motion, you know the direction of its movement.<br/>The spell can locate a specific object known to you, as long as you have seen it up close--within 30 feet--at least once. Alternatively, the spell can locate the nearest object of a particular kind, such as a certain kind of apparel, jewelry, furniture, tool, or weapon.<br/>This spell can't locate an object if any thickness of lead, even a thin sheet, blocks a direct path between you and the object."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Magic Mouth"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a small bit of honeycomb and jade dust worth at least 10 gp, which the spell consumes)"
    , spDuration = "Until dispelled"
    , spDescription = "You implant a message within an object in range, a message that is uttered when a trigger condition is met. Choose an object that you can see and that isn't being worn or carried by another creature. Then speak the message, which must be 25 words or less, though it can be delivered over as long as 10 minutes. Finally, determine the circumstance that will trigger the spell to deliver your message.<br/>When that circumstance occurs, a magical mouth appears on the object and recites the message in your voice and at the same volume you spoke. If the object you chose has a mouth or something that looks like a mouth (for example, the mouth of a statue), the magical mouth appears there so that the words appear to come from the object's mouth. When you cast this spell, you can have the spell end after it delivers its message, or it can remain and repeat its message whenever the trigger occurs.<br/>The triggering circumstance can be as general or as detailed as you like, though it must be based on visual or audible conditions that occur within 30 feet of the object. For example, you could instruct the mouth to speak when any creature moves within 30 feet of the object or when a silver bell rings within 30 feet of it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Magic Weapon"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You touch a nonmagical weapon. Until the spell ends, that weapon becomes a magic weapon with a +1 bonus to attack rolls and damage rolls."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, the bonus increases to +2. When you use a spell slot of 6th level or higher, the bonus increases to +3."
    }
  , Spell
    { spName = "Maximillian's Earthen Grasp"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a miniature hand sculpted from clay)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You choose a 5-foot-square unoccupied space on the ground that you can see within range. A Medium hand made from compacted soil rises there and reaches for one creature you can see within 5 feet of it. The target must make a Strength saving throw. On a failed save, the target takes 2d6 bludgeoning damage and is <i>restrained</i> for the spell’s duration.<br/>As an action, you can cause the hand to crush the <i>restrained</i> target, who must make a Strength saving throw. It takes 2d6 bludgeoning damage on a failed save, or half as much damage on a successful one.<br/>To break out, the <i>restrained</i> target can use its action to make a Strength check against your spell save DC. On a success, the target escapes and is no longer <i>restrained</i> by the hand.<br/>As an action, you can cause the hand to reach for a different creature or to move to a different unoccupied space within range. The hand releases a <i>restrained</i> target if you do either."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Melf's Acid Arrow"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (powdered rhubarb and an adder's stomach)"
    , spDuration = "Instantaneous"
    , spDescription = "A shimmering green arrow streaks toward a target within range and bursts in a spray of acid. Make a ranged spell attack against the target. On a hit, the target takes 4d4 acid damage immediately and 2d4 acid damage at the end of its next turn. On a miss, the arrow splashes the target with acid for half as much of the initial damage and no damage at the end of its next turn."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage (both initial and later) increases by 1d4 for each slot level above 2nd."
    }
  , Spell
    { spName = "Mind Spike"
    , spLevel = Two
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You reach into the mind of one creature you can see within range. The target must make a Wisdom saving throw, taking 3d8 psychic damage on a failed save, or half as much damage on a successful one. On a failed save, you also always know the target's location until the spell ends, but only while the two of you are on the same plane of existence. While you have this knowledge, the target can't become hidden from you, and if it's invisible, it gains no benefit from that condition against you."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d6 for each slot level above 2nd."
    }
  , Spell
    { spName = "Mind Thrust (Unearthed Arcana)"
    , spLevel = Two
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V"
    , spDuration = "1 round"
    , spDescription = "You thrust a lance of psychic disruption into the mind of one creature you can see within range. The target must make an Intelligence saving throw. On a failed save, the target takes 3d6 psychic damage, and it can’t take a reaction until the end of its next turn. Moreover, on its next turn, it must choose whether it gets a move, an action, or a bonus action; it gets only one of the three. On a successful save, the target takes half as much damage and suffers none of the spell’s other effects."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd. The creatures must be within 30 feet of each other when you target them."
    }
  , Spell
    { spName = "Mirror Image"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spDescription = "Three illusory duplicates of yourself appear in your space. Until the spell ends, the duplicates move with you and mimic your actions, shifting position so it's impossible to track which image is real. You can use your action to dismiss the illusory duplicates.<br/>"
      <> "Each time a creature targets you with an attack during the spell's duration, roll a d20 to determine whether the attack instead targets one of your duplicates.<br/>If you have three duplicates, you must roll a 6 or higher to change the attack's target to a duplicate. With two duplicates, you must roll an 8 or higher. With one duplicate, you must roll an 11 or higher.<br/>"
      <> "A duplicate's AC equals 10 + your Dexterity modifier. If an attack hits a duplicate, the duplicate is destroyed. A duplicate can be destroyed only by an attack that hits it. It ignores all other damage and effects. The spell ends when all three duplicates are destroyed.<br/>"
      <> "A creature is unaffected by this spell if it can't see, if it relies on senses other than sight, such as <i>blindsight</i>, or if it can perceive illusions as false, as with <i>truesight</i>."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Misty Step"
    , spLevel = Two
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Self"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spDescription = "Briefly surrounded by silvery mist, you teleport up to 30 feet to an unoccupied space that you can see."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Nystul's Magic Aura"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a small square of silk)"
    , spDuration = "24 hours"
    , spDescription = "You place an illusion on a creature or an object you touch so that divination spells reveal false information about it. The target can be a willing creature or an object that isn't being carried or worn by another creature.<br/>When you cast the spell, choose one or both of the following effects. The effect lasts for the duration. If you cast this spell on the same creature or object every day for 30 days, placing the same effect on it each time, the illusion lasts until it is dispelled.<br/><b>False Aura.</b> You change the way the target appears to spells and magical effects, such as <i>detect magic</i>, that detect magical auras. You can make a nonmagical object appear magical, a magical object appear nonmagical, or change the object's magical aura so that it appears to belong to a specific school of magic that you choose. When you use this effect on an object, you can make the false magic apparent to any creature that handles the item.<br/><b>Mask.</b> You change the way the target appears to spells and magical effects that detect creature types, such as a paladin's Divine Sense or the trigger of a <i>symbol</i> spell. You choose a creature type and other spells and magical effects treat the target as if it were a creature of that type or of that alignment."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Phantasmal Force"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of fleece)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You craft an illusion that takes root in the mind of a creature that you can see within range. The target must make an Intelligence saving throw. On a failed save, you create a phantasmal object, creature, or other visible phenomenon of your choice that is no larger than a 10-foot cube and that is perceivable only to the target for the duration. The spell has no effect on undead or constructs.<br/>The phantasm includes sound, temperature and other stimuli, also evident only to the creature.<br/>The target can use its action to examine the phantasm as if it were real. The target rationalizes any illogical outcomes from interacting with the phantasm. For example, a target attempting to walk across a phantasmal bridge that spans a chasm falls once it steps on to the bridge. If the target survives the fall, it still believes that the bridge exists and comes up with some other explanation for its fall - it was pushed, it slipped, or a strong wind might have knocked it off.<br/>An affected target is so convinced of the phantasm's reality that it can even take damage from the illusion. A phantasm created to appear as fire, a pool of acid, or lava can burn the target. Each round on your turn, the phantasm can deal 1d6 psychic damage to the target if it is in the phantasm's area or within 5 feet of the phantasm, provided that the illusion is of a creature or hazard that could logically deal damage, such as by attacking. The target perceives the damage as a type appropriate to the illusion."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Protection from Poison"
    , spLevel = Two
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "1 hour"
    , spDescription = "You touch a creature. If it is poisoned, you neutralize the poison. If more than one poison afflicts the target, you neutralize one poison that you know is present, or you neutralize one at random.<br/>"
      <> "For the duration, the target has advantage on saving throws against being poisoned, and it has resistance to poison damage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Pyrotechnics"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "Choose an area of nonmagical flame that you can see and that fits within a 5-foot cube within range. You can extinguish the fire in that area, and you create either fireworks or smoke when you do so.<br/>"
      <> "<b>Fireworks.</b> The target explodes with a dazzling display of colors. Each creature within 10 feet of the target must succeed on a Constitution saving throw or become <i>blinded</i> until the end of your next turn.<br/>"
      <> "<b>Smoke.</b> Thick black smoke spreads out from the target in a 20-foot radius, moving around corners. The area of the smoke is heavily obscured. The smoke persists for 1 minute or until a strong wind disperses it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Ray of Enfeeblement"
    , spLevel = Two
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A black beam of enervating energy springs from your finger toward a creature within range. Make a ranged spell attack against the target. On a hit, the target deals only half damage with weapon attacks that use Strength until the spell ends.<br/>At the end of each of the target's turns, it can make a Constitution saving throw against the spell. On a success, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Rope Trick"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (powdered corn extract and a twisted loop of parchment)"
    , spDuration = "1 hour"
    , spDescription = "You touch a length of rope that is up to 60 feet long. One end of the rope then rises into the air until the whole rope hangs perpendicular to the ground. At the upper end of the rope, an invisible entrance opens to an extradimensional space that lasts until the spell ends.<br/>The extradimensional space can be reached by climbing to the top of the rope. The space can hold as many as eight Medium or smaller creatures. The rope can be pulled into the space, making the rope disappear from view outside the space.<br/>Attacks and spells can't cross through the entrance into or out of the extradimensional space, but those inside can see out of it as if through a 3-foot-by-5- foot window centered on the rope.<br/>Anything inside the extradimensional space drops out when the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Scorching Ray"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You create three rays of fire and hurl them at targets within range. You can hurl them at one target or several.<br/>Make a ranged spell attack for each ray. On a hit, the target takes 2d6 fire damage."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you create one additional ray for each slot level above 2nd."
    }
  , Spell
    { spName = "See Invisibility"
    , spLevel = Two
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a pinch of talc and a small sprinkling of powdered silver)"
    , spDuration = "1 hour"
    , spDescription = "For the duration, you see <i>invisible</i> creatures and objects as if they were visible, and you can see into the Ethereal Plane. Ethereal creatures and objects appear ghostly and translucent."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Shadow Blade (Xanathar's)"
    , spLevel = Two
    , spType = Illusion
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You weave together threads of shadow to create a sword of solidified gloom in your hand. The magic sword lasts until the spell ends. It counts as a simple melee weapon with which you are proficient. It deals 2d8 psychic damage on a hit and has the finesse, light and thrown properties (range 20/60). In addition, when you use the sword to attack a target that is in dim light or darkness, you make the attack roll with advantage.<br/>If you drop the weapon or throw it, it dissapates at the end of the turn. Thereafter, while the spell persists, you can use your bonus action to cause the sword to reappear in your hand."
    , spHigher = Just "When you cast this spell using a 3rd or 4th level slot, the damage increases to 3d8. When you cast this spell using a 5th or 6th level slot, the damage increases to 4d8. When you cast this spell using a spell slot of 7th level or higher, the damage increases to 5d8."
    }
  , Spell
    { spName = "Shatter"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a chip of mica)"
    , spDuration = "Instantaneous"
    , spDescription = "A sudden loud ringing noise, painfully intense, erupts from a point of your choice within range. Each creature in a 10-foot-radius sphere centered on that point must make a Constitution saving throw. A creature takes 3d8 thunder damage on a failed save, or half as much damage on a successful one. A creature made of inorganic material such as stone, crystal, or metal has disadvantage on this saving throw.<br/>A nonmagical object that isn't being worn or carried also takes the damage if it's in the spell's area."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
  , Spell
    { spName = "Silence"
    , spLevel = Two
    , spType = Illusion
    , spRitual = True
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "For the duration, no sound can be created within or pass through a 20-foot-radius sphere centered on a point you choose within range. Any creature or object entirely inside the sphere is immune to thunder damage, and creatures are deafened while entirely inside it. Casting a spell that includes a verbal component is impossible there."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Skywrite"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Sight"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You cause up to ten words to form in a part of the sky you can see. The words appear to be made of cloud and remain in place for the spell’s duration. The words dissipate when the spell ends. A strong wind can disperse the clouds and end the spell early"
    , spHigher = Nothing
    }
  , Spell
    { spName = "Snilloc's Snowball Swarm"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (a piece of ice or a small white rock chip)"
    , spDuration = "Instantaneous"
    , spDescription = "A flurry of magic snowballs erupts from a point you choose within range. Each creature in a 5-foot-radius sphere centered on that point must make a Dexterity saving throw. A creature takes 3d6 cold damage on a failed save, or half as much damage on a successful one."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d6 for each slot level above 2nd."
    }
  , Spell
    { spName = "Spider Climb"
    , spLevel = Two
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a drop of bitumen and a spider)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "Until the spell ends, one willing creature you touch gains the ability to move up, down, and across vertical surfaces and upside down along ceilings, while leaving its hands free. The target also gains a climbing speed equal to its walking speed."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Suggestion"
    , spLevel = Two
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 8 hours"
    , spDescription = "You suggest a course of activity (limited to a sentence or two) and magically influence a creature you can see within range that can hear and understand you. Creatures that can't be <i>charmed</i> are immune to this effect. The suggestion must be worded in such a manner as to make the course of action sound reasonable. Asking the creature to stab itself, throw itself onto a spear, immolate itself, or do some other obviously harmful act ends the spell.<br/>The target must make a Wisdom saving throw. On a failed save, it pursues the course of action you described to the best of its ability. The suggested course of action can continue for the entire duration. If the suggested activity can be completed in a shorter time, the spell ends when the subject finishes what it was asked to do.<br/>You can also specify conditions that will trigger a special activity during the duration. For example, you might suggest that a knight give her warhorse to the first beggar she meets. If the condition isn't met before the spell expires, the activity isn't performed.<br/>If you or any of your companions damage the target, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Warding Wind"
    , spLevel = Two
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "A strong wind (20 miles per hour) blows around you in a 10-foot radius and moves with you, remaining centered on you. The wind lasts for the spell’s duration.<br/>The wind has the following effects: <ul><li>It deafens you and other creatures in its area.</li><li>It extinguishes unprotected flames in its area that are torch-sized or smaller.</li><li>It hedges out vapor, gas, and fog that can be dispersed by strong wind.</li><li>The area is difficult terrain for creatures other than you.</li><li>The attack rolls of ranged weapon attacks have disadvantage if the attacks pass in or out of the wind.</li></ul>"
    , spHigher = Nothing
    }
  , Spell
    { spName = "Web"
    , spLevel = Two
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of spiderweb)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You conjure a mass of thick, sticky webbing at a point of your choice within range. The webs fill a 20-foot cube from that point for the duration. The webs are difficult terrain and lightly obscure their area.<br/>If the webs aren't anchored between two solid masses (such as walls or trees) or layered across a floor, wall, or ceiling, the conjured web collapses on itself, and the spell ends at the start of your next turn. Webs layered over a flat surface have a depth of 5 feet.<br/>Each creature that starts its turn in the webs or that enters them during its turn must make a Dexterity saving throw. On a failed save, the creature is <i>restrained</i> as long as it remains in the webs or until it breaks free.<br/>A creature <i>restrained</i> by the webs can use its action to make a Strength check against your spell save DC. If it succeeds, it is no longer restrained.<br/>The webs are flammable. Any 5-foot cube of webs exposed to fire burns away in 1 round, dealing 2d4 fire damage to any creature that starts its turn in the fire."
    , spHigher = Nothing
    }
  ]

thirdLevel :: [Spell]
thirdLevel =
  [ Spell
    { spName = "Blink"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spDescription = "Roll a d20 at the end of each of your turns for the duration of the spell. On a roll of 11 or higher, you vanish from your current plane of existence and appear in the Ethereal Plane (the spell fails and the casting is wasted if you were already on that plane). At the start of your next turn, and when the spell ends if you are on the Ethereal Plane, you return to an unoccupied space of your choice that you can see within 10 feet of the space you vanished from. If no unoccupied space is available within that range, you appear in the nearest unoccupied space (chosen at random if more than one space is equally near). You can dismiss this spell as an action.<br/>" <>
      "While on the Ethereal Plane, you can see and hear the plane you originated from, which is cast in shades of gray, and you can't see anything there more than 60 feet away. You can only affect and be affected by other creatures on the Ethereal Plane. Creatures that aren't there can't perceive you or interact with you, unless they have the ability to do so."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Call Lightning"
    , spLevel = Three
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "A storm cloud appears in the shape of a cylinder that is 10 feet tall with a 60-foot radius, centered on a point you can see within range directly above you. The spell fails if you can’t see a point in the air where the storm cloud could appear (for example, if you are in a room that can’t accommodate the cloud).<br/>" <>
      "When you cast the spell, choose a point you can see under the cloud. A bolt of lightning flashes down from the cloud to that point. Each creature within 5 feet of that point must make a Dexterity saving throw. A creature takes 3d10 lightning damage on a failed save, or half as much damage on a successful one. On each of your turns until the spell ends, you can use your action to call down lightning in this way again, targeting the same point or a different one.<br/>" <>
      "If you are outdoors in stormy conditions when you cast this spell, the spell gives you control over the existing storm instead of creating a new one. Under such conditions, the spell’s damage increases by 1d10."
    , spHigher = Just "When you cast this spell using a spell slot of 4th or higher level, the damage increases by 1d10 for each slot level above 3rd."
    }
  , Spell
    { spName = "Catnap"
    , spLevel = Three
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "S,M (a pinch of sand)"
    , spDuration = "10 minutes"
    , spDescription = "You make a calming gesture, and up to three willing creatures of your choice that you can see within range fall <i>unconscious</i> for the spell's duration. The spell ends on a target early if it takes damage or someone uses an action to shake or slap it awake. If a target remains <i>unconscious</i> for the full duration, that target gains the benefit of a short rest, and it can't be affected by this spell again until it finishes a long rest."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, you can target one additional willing creature for each slot level above 3rd."
    }
  , Spell
    { spName = "Create Food and Water"
    , spLevel = Three
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You create 45 pounds of food and 30 gallons of water on the ground or in containers within range, enough to sustain up to fifteen humanoids or five steeds for 24 hours. The food is bland but nourishing, and spoils if uneaten after 24 hours. The water is clean and doesn't go bad."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Dispel Magic"
    , spLevel = Three
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "Choose one creature, object, or magical effect within range. Any spell of 3rd level or lower on the target ends. For each spell of 4th level or higher on the target, make an ability check using your spellcasting ability. The DC equals 10 + the spell's level. On a successful check, the spell ends."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, you automatically end the effects of a spell on the target if the spell's level is equal to or less than the level of the spell slot you used."
    }
  , Spell
    { spName = "Elemental Weapon"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "A nonmagical weapon you touch becomes a magic weapon.  Choose one of the following damage types: acid, cold, fire, lightning, or thunder. For the duration, the weapon has a +1 bonus to attack rolls and deals an extra 1d4 damage of the chosen type when it hits."
    , spHigher = Just "When you cast this spell using a spell slot of 5th or 6th level, the bonus to attack rolls increases to +2 and the extra damage increases to 2d4. When you use a spell slot of 7th level or higher, the bonus increases to +3 and the extra damage increases to 3d4."
    }
  , Spell
    { spName = "Fireball"
    , spLevel = Three
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "150 feet"
    , spComponents = "V,S,M (a tiny ball of bat guano and sulfur)"
    , spDuration = "Instantaneous"
    , spDescription = "A bright streak flashes from your pointing finger to a point you choose within range and then blossoms with a low roar into an explosion of flame. Each creature in a 20-foot-radius sphere centered on that point must make a Dexterity saving throw. A target takes 8d6 fire damage on a failed save, or half as much damage on a successful one.<br/>"
      <> "The fire spreads around corners. It ignites flammable objects in the area that aren't being worn or carried."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1d6 for each slot level above 3rd."
    }
  , Spell
    { spName = "Flame Arrows"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You touch a quiver containing arrows or bolts. When a target is hit by a ranged weapon attack using a piece of ammunition drawn from the quiver, the target takes an extra 1d6 fire damage. The spell’s magic ends on the piece of ammunition when it hits or misses, and the spell ends when twelve pieces of ammunition have been drawn from the quiver."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, the number of pieces of ammunition you can affect with this spell increases by two for each slot level above 3rd."
    }
  , Spell
    { spName = "Fly"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "You touch a willing creature. The target gains a flying speed of 60 feet for the duration. When the spell ends, the target falls if it is still aloft, unless it can stop the fall."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, you can target one additional creature for each slot level above 3rd."
    }
  , Spell
    { spName = "Glyph of Warding"
    , spLevel = Three
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 hour"
    , spRange = "Touch"
    , spComponents = "V,S,M (incense and powdered diamond worth at least 200 gp, which the spell consumes)"
    , spDuration = "Until Dispelled or Triggered"
    , spDescription = "When you cast this spell, you inscribe a glyph that later unleashes a magical effect. You inscribe it either on a surface (such as a table or a section of floor or wall) or within an object that can be closed (such as a book, a scroll, or a treasure chest) to conceal the glyph. The glyph can cover an area no larger than 10 feet in diameter. If the surface or object is moved more than 10 feet from where you cast this spell, the glyph is broken, and the spell ends without being triggered.<br/>"
      <> "The glyph is nearly invisible and requires a successful Intelligence (Investigation) check against your spell save DC to be found.<br/>"
      <> "You decide what triggers the glyph when you cast the spell. For glyphs inscribed on a surface, the most typical triggers include touching or standing on the glyph, removing another object covering the glyph, approaching within a certain distance of the glyph, or manipulating the object on which the glyph is inscribed. For glyphs inscribed within an object, the most common triggers include opening that object, approaching within a certain distance of the object, or seeing or reading the glyph. Once a glyph is triggered, this spell ends.<br/>"
      <> "You can further refine the trigger so the spell activates only under certain circumstances or according to physical characteristics (such as height or weight), creature kind (for example, the ward could be set to affect aberrations or drow), or alignment. You can also set conditions for creatures that don’t trigger the glyph, such as those who say a certain password.<br/>"
      <> "When you inscribe the glyph, choose explosive runes or a spell glyph.<br/>"
      <> "<b><i>Explosive Runes.</i></b> When triggered, the glyph erupts with magical energy in a 20-foot-radius sphere centered on the glyph. The sphere spreads around corners. Each creature in the area must make a Dexterity saving throw. A creature takes 5d8 acid, cold, fire, lightning, or thunder damage on a failed saving throw (your choice when you create the glyph), or half as much damage on a successful one.<br/>"
      <> "<b><i>Spell Glyph.</i></b> You can store a prepared spell of 3rd level or lower in the glyph by casting it as part of creating the glyph. The spell must target a single creature or an area. The spell being stored has no immediate effect when cast in this way. When the glyph is triggered, the stored spell is cast. If the spell has a target, it targets the creature that triggered the glyph. If the spell affects an area, the area is centered on that creature. If the spell summons hostile creatures or creates harmful objects or traps, they appear as close as possible to the intruder and attack it. If the spell requires concentration, it lasts until the end of its full duration."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, the damage of an <i>explosive runes</i> glyph increases by 1d8 for each slot level above 3rd. If you create a <i>spell glyph</i>, you can store any spell of up to the same level as the slot you use for the glyph of warding."
    }
  , Spell
    { spName = "Haste"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a shaving of licorice root)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose a willing creature that you can see within range. Until the spell ends, the target's speed is doubled, it gains a +2 bonus to AC, it has advantage on Dexterity saving throws, and it gains an additional action on each of its turns. That action can be used only to take the Attack (one weapon attack only), Dash, Disengage, Hide, or Use an Object action.<br/>"
      <> "When the spell ends, the target can't move or take actions until after its next turn, as a wave of lethargy sweeps over it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Hypnotic Pattern"
    , spLevel = Three
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "S,M (a glowing stick of incense or a crystal vial filled with phosphorescent material)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You create a twisting pattern of colors that weaves through the air inside a 30-foot cube within range. The pattern appears for a moment and vanishes. Each creature in the area who sees the pattern must make a Wisdom saving throw. On a failed save, the creature becomes charmed for the duration. While charmed by this spell, the creature is incapacitated and has a speed of 0.<br/>"
      <> "The spell ends for an affected creature if it takes any damage or if someone else uses an action to shake the creature out of its stupor."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Intellect Fortress"
    , spLevel = Three
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "For the duration, you or one willing creature you can see within range has resistance to psychic damage, as well as advantage on Intelligence, Wisdom, and Charisma saving throws."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, you can target one additional creature for each slot level above 3rd. The creatures must be within 30 feet of each other when you target them."
    }
  , Spell
    { spName = "Protection from Energy"
    , spLevel = Three
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "For the duration, the willing creature you touch has resistance to one damage type of your choice: acid, cold, fire, lightning, or thunder."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Revivify"
    , spLevel = Three
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (diamonds worth 300 gp, which the spell consumes)"
    , spDuration = "Instantaneous"
    , spDescription = "You touch a creature that has died within the last minute. That creature returns to life with 1 hit point. This spell can't return to life a creature that has died of old age, nor can it restore any missing body parts."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Slow"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a drop of molasses)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You alter time around up to six creatures of your choice in a 40-foot cube within range. Each target must succeed on a Wisdom saving throw or be affected by this spell for the duration.<br/>"
      <> "An affected target's speed is halved, it takes a -2 penalty to AC and Dexterity saving throws, and it can't use reactions. On its turn, it can use either an action or a bonus action, not both. Regardless of the creature's abilities or magic items, it can't make more than one melee or ranged attack during its turn.<br/>"
      <> "If the creature attempts to cast a spell with a casting time of 1 action, roll a d20. On an 11 or higher, the spell doesn't take effect until the creature's next turn, and the creature must use its action on that turn to complete the spell. If it can't, the spell is wasted.<br/>"
      <> "A creature affected by this spell makes another Wisdom saving throw at the end of each of its turns. On a successful save, the effect ends for it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Tiny Servant"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "8 hours"
    , spDescription = "You touch one Tiny, nonmagical object that isnt attached to another object or a surface and isnt being carried by another creature. The target animates and sprouts little arms and legs, becoming a creature under your control until the spell ends or the creature drops to 0 hit points. See Tiny Servant for its statistics.<br/>"
      <> "As a bonus action, you can mentally command the creature if it is within 120 feet of you. (If you control multiple creatures with this spell, you can command any or all of them at the same time, issuing the same command to each one.) You decide what action the creature will take and where it will move during its next turn, or you can issue a simple, general command, such as to fetch a key, stand watch, or stack some books. If you issue no commands, the servant does nothing other than defend itself against hostile creatures. Once given an order, the servant continues to follow that order until its task is complete.<br/>"
      <> "When the creature drops to 0 hit points, it reverts to its original form, and any remaining damage carries over to that form."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, you can animate two additional objects for each slot level above 3rd."
    }
  , Spell
    { spName = "Water Breathing"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a short reed or piece of straw)"
    , spDuration = "24 hours"
    , spDescription = "This spell grants up to ten willing creatures you can see within range the ability to breathe underwater until the spell ends. Affected creatures also retain their normal mode of respiration."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Water Walk"
    , spLevel = Three
    , spType = Transmutation
    , spRitual = True
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a piece of cork)"
    , spDuration = "1 hour"
    , spDescription = "This spell grants the ability to move across any liquid surface--such as water, acid, mud, snow, quicksand, or lava--as if it were harmless solid ground (creatures crossing molten lava can still take damage from the heat). Up to ten willing creatures you can see within range gain this ability for the duration.<br/>"
      <> "If you target a creature submerged in a liquid, the spell carries the target to the surface of the liquid at a rate of 60 feet per round."
    , spHigher = Nothing
    }
  ]

fourthLevel :: [Spell]
fourthLevel =
  [ Spell
    { spName = "Arcane Eye"
    , spLevel = Four
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a bit of bat fur)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You create an invisible, magical eye within range that hovers in the air for the duration.<br/>" <>
      "You mentally receive visual information from the eye, which has normal vision and <i>darkvision</i> out to 30 feet. The eye can look in every direction.<br/>" <>
      "As an action, you can move the eye up to 30 feet in any direction. There is no limit to how far away from you the eye can move, but it can't enter another plane of existence. A solid barrier blocks the eye's movement, but the eye can pass through an opening as small as 1 inch in diameter."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Conj. Minor Elementals"
    , spLevel = Four
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "90 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You summon elementals that appear in unoccupied spaces that you can see within range. You choose one the following options for what appears:<br/>" <>
      "- One elemental of challenge rating 2 or lower<br/>" <>
      "- Two elemental of challenge rating 1 or lower<br/>" <>
      "- Four elemental of challenge rating 1/2 or lower<br/>" <>
      "- Eight elemental of challenge rating 1/4 or lower<br/>" <>
      "An elemental summoned by this spell disappears when it drops to 0 hit points or when the spell ends.<br/>" <>
      "The summoned creatures are friendly to you and your companions. Roll initiative for the summoned creatures as a group, which has its own turns. They obey any verbal commands that you issue to them (no action required by you). If you don't issue any commands to them, they defend themselves from hostile creatures, but otherwise take no actions.<br/>" <>
      "The GM has the creatures' statistics.<br/>" <>
      "<table><tr><th> CR  </th><th> Monster </th></tr>" <>
      "<tr><td> 1/4 </td><td> Steam Mephit </td></tr>" <>
      "<tr><td> 1/2 </td><td> Dust Mephit, Ice Mephit, Magma Mephit, Magmin </td></tr>" <>
      "<tr><td> 2   </td><td> Azer, Gargoyle </td></tr></table>"
    , spHigher = Just "When you cast this spell using certain higher-level spell slots, you choose one of the summoning options above, and more creatures appear: twice as many with a 6th-level slot and three times as many with an 8th-level slot."
    }
  , Spell
    { spName = "Divination"
    , spLevel = Four
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (incense and a sacrificial offering appropriate to your religion, together worth at least 25 gp, which the spell consumes)"
    , spDuration = "Instantaneous"
    , spDescription = "Your magic and an offering put you in contact with a god or a god's servants. You ask a single question concerning a specific goal, event, or activity to occur within 7 days. The GM offers a truthful reply. The reply might be a short phrase, a cryptic rhyme, or an omen.<br/>"
      <> "The spell doesn't take into account any possible circumstances that might change the outcome, such as the casting of additional spells or the loss or gain of a companion.<br/>"
      <> "If you cast the spell two or more times before finishing your next long rest, there is a cumulative 25 percent chance for each casting after the first that you get a random reading. The GM makes this roll in secret."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Elemental Bane"
    , spLevel = Four
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose one creature you can see within range, and choose one of the following damage types: acid, cold, fire, lightning, or thunder. The target must succeed on a Constitution saving throw or be affected by the spell for its duration. The first time each turn the affected target takes damage of the chosen type, the target takes an extra 2d6 damage of that type. Moreover, the target loses any resistance to that damage type until the spell ends."
    , spHigher = Just "When you cast this spell using a spell slot of 5th level or higher, you can target one additional creature for each slot level above 4th. The creatures must be within 30 feet of each other when you target them."
    }
  , Spell
    { spName = "Fabricate"
    , spLevel = Four
    , spType = Transmutation
    , spRitual = False
    , spTime = "10 minutes"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You convert raw materials into products of the same material. For example, you can fabricate a wooden bridge from a clump of trees, a rope from a patch of hemp, and clothes from flax or wool.<br/>"
      <> "Choose raw materials that you can see within range. You can fabricate a Large or smaller object (contained within a 10-foot cube, or eight connected 5-foot cubes), given a sufficient quantity of raw material. If you are working with metal, stone, or another mineral substance, however, the fabricated object can be no larger than Medium (contained within a single 5-foot cube). The quality of objects made by the spell is commensurate with the quality of the raw materials.<br/>"
      <> "Creatures or magic items can't be created or transmuted by this spell. You also can't use it to create items that ordinarily require a high degree of craftsmanship, such as jewelry, weapons, glass, or armor, unless you have proficiency with the type of artisan's tools used to craft such objects."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Find Greater Steed"
    , spLevel = Four
    , spType = Conjuration
    , spRitual = False
    , spTime = "10 minutes"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You summon a spirit that assumes the form of a loyal, majestic mount. Appearing in an unoccupied space within range, the spirit takes on a form you choose: a griffon, a pegasus, a peryton, a dire wolf, a rhinoceros, or a saber-toothed tiger. The creature has the statistics provided in the Monster Manual for the chosen form, though it is a celestial, a fey, or a fiend (your choice) instead of its normal creature type. Additionally, if it has an Intelligence score of 5 or lower, its Intelligence becomes 6, and it gains the ability to understand one language of your choice that you speak.<br/>"
      <> "You control the mount in combat. While the mount is within 1 mile of you, you can communicate with it telepathically. While mounted on it, you can make any spell you cast that targets only you also target the mount.<br/>"
      <> "The mount disappears temporarily when it drops to 0 hit points or when you dismiss it as an action. Casting this spell again re-summons the bonded mount, with all its hit points restored and any conditions removed.<br/>"
      <> "You can't have more than one mount bonded by this spell or find steed at the same time. As an action, you can release a mount from its bond, causing it to disappear permanently.<br/>"
      <> "Whenever the mount disappears, it leaves behind any objects it was wearing or carrying."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Fire Shield"
    , spLevel = Four
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a bit of phosphorus or a firefly)"
    , spDuration = "10 minutes"
    , spDescription = "Thin and wispy flames wreathe your body for the duration, shedding bright light in a 10-foot radius and dim light for an additional 10 feet. You can end the spell early by using an action to dismiss it.<br/>"
      <> "The flames provide you with a warm shield or a chill shield, as you choose. The warm shield grants you resistance to cold damage, and the chill shield grants you resistance to fire damage.<br/>"
      <> "In addition, whenever a creature within 5 feet of you hits you with a melee attack, the shield erupts with flame. The attacker takes 2d8 fire damage from a warm shield, or 2d8 cold damage from a cold shield."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Freedom of Movement"
    , spLevel = Four
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a leather strap, bound around the arm or a similar appendage)"
    , spDuration = "1 hour"
    , spDescription = "You touch a willing creature. For the duration, the target's movement is unaffected by difficult terrain, and spells and other magical effects can neither reduce the target's speed nor cause the target to be paralyzed or restrained.<br/>"
      <> "The target can also spend 5 feet of movement to automatically escape from nonmagical restraints, such as manacles or a creature that has it grappled. Finally, being underwater imposes no penalties on the target's movement or attacks."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Greater Invisibility"
    , spLevel = Four
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You or a creature you touch becomes <i>invisible</i> until the spell ends. Anything the target is wearing or carrying is <i>invisible</i> as long as it is on the target's person."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Leomunds Secret Chest"
    , spLevel = Four
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (an exquisite chest, 3 feet by 2 feet by 2 feet, constructed from rare materials worth at least 5,000 gp, and a Tiny replica made from the same materials worth at least 50 gp)"
    , spDuration = "Instantaneous"
    , spDescription = "You hide a chest, and all its contents, on the Ethereal Plane. You must touch the chest and the miniature replica that serves as a material component for the spell. The chest can contain up to 12 cubic feet of nonliving material (3 feet by 2 feet by 2 feet).<br/>"
      <> "While the chest remains on the Ethereal Plane, you can use an action and touch the replica to recall the chest. It appears in an unoccupied space on the ground within 5 feet of you. You can send the chest back to the Ethereal Plane by using an action and touching both the chest and the replica.<br/>"
      <> "After 60 days, there is a cumulative 5 percent chance per day that the spell’s effect ends. This effect ends if you cast this spell again, if the smaller replica chest is destroyed, or if you choose to end the spell as an action. If the spell ends and the larger chest is on the Ethereal Plane, it is irretrievably lost."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Faithful Hound"
    , spLevel = Four
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a tiny silver whistle, a piece of bone, and a thread)"
    , spDuration = "8 hours"
    , spDescription = "You conjure a phantom watchdog in an unoccupied space that you can see within range, where it remains for the duration, until you dismiss it as an action, or until you move more than 100 feet away from it.<br/>"
      <> "The hound is invisible to all creatures except you and can't be harmed. When a Small or larger creature comes within 30 feet of it without first speaking the password that you specify when you cast this spell, the hound starts barking loudly. The hound sees invisible creatures and can see into the Ethereal Plane. It ignores illusions.<br/>"
      <> "At the start of each of your turns, the hound attempts to bite one creature within 5 feet of it that is hostile to you. The hound's attack bonus is equal to your spellcasting ability modifier + your proficiency bonus. On a hit, it deals 4d8 piercing damage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Private Sanctum"
    , spLevel = Four
    , spType = Abjuration
    , spRitual = False
    , spTime = "10 minutes"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a thin sheet of lead, a piece of opaque glass, a wad of cotton or cloth, and powdered chrysolite)"
    , spDuration = "24 hours"
    , spDescription = "You make an area within range magically secure. The area is a cube that can be as small as 5 feet to as large as 100 feet on each side. The spell lasts for the duration or until you use an action to dismiss it.<br/>"
      <> "When you cast the spell, you decide what sort of security the spell provides, choosing any or all of the following properties:<br/>"
      <> "- Sound can't pass through the barrier at the edge of the warded area.\n"
      <> "- The barrier of the warded area appears dark and foggy, preventing vision (including darkvision) through it.\n"
      <> "- Sensors created by divination spells can't appear inside the protected area or pass through the barrier at its perimeter.\n"
      <> "- Creatures in the area can't be targeted by divination spells.\n"
      <> "- Nothing can teleport into or out of the warded area.\n"
      <> "- Planar travel is blocked within the warded area.\n"
      <> "Casting this spell on the same spot every day for a year makes this effect permanent."
    , spHigher = Just "When you cast this spell using a spell slot of 5th level or higher, you can increase the size of the cube by 100 feet for each slot level beyond 4th. Thus you could protect a cube that can be up to 200 feet on one side by using a spell slot of 5th level."
    }
  , Spell
    { spName = "Resilient Sphere"
    , spLevel = Four
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a hemispherical piece of clear crystal and a matching hemispherical piece of gum arabic)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A sphere of shimmering force encloses a creature or object of Large size or smaller within range. An unwilling creature must make a Dexterity saving throw. On a failed save, the creature is enclosed for the duration.<br/>"
      <> "Nothing—not physical objects, energy, or other spell effects—can pass through the barrier, in or out, though a creature in the sphere can breathe there. The sphere is immune to all damage, and a creature or object inside can’t be damaged by attacks or effects originating from outside, nor can a creature inside the sphere damage anything outside it.<br/>"
      <> "The sphere is weightless and just large enough to contain the creature or object inside. An enclosed creature can use its action to push against the sphere’s walls and thus roll the sphere at up to half the creature’s speed. Similarly, the globe can be picked up and moved by other creatures.<br/>"
      <> "A disintegrate spell targeting the globe destroys it without harming anything inside it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Stone Shape"
    , spLevel = Four
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (soft clay, which must be worked into roughly the desired shape of the stone object)"
    , spDuration = "Instantaneous"
    , spDescription = "You touch a stone object of Medium size or smaller or a section of stone no more than 5 feet in any dimension and form it into any shape that suits your purpose. So, for example, you could shape a large rock into a weapon, idol, or coffer, or make a small passage through a wall, as long as the wall is less than 5 feet thick. You could also shape a stone door or its frame to seal the door shut. The object you create can have up to two hinges and a latch, but finer mechanical detail isn't possible."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Stoneskin"
    , spLevel = Four
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (diamond dust worth 100 gp, which the spell consumes)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "This spell turns the flesh of a willing creature you touch as hard as stone. Until the spell ends, the target has resistance to nonmagical bludgeoning, piercing, and slashing damage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Summon Construct"
    , spLevel = Four
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (an ornate stone and metal lockbox worth at least 400 gp)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You call forth the spirit of a construct. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Construct Spirit stat block. When you cast the spell, choose a material: Clay, Metal, or Stone. The creature resembles a golem or a modron (your choice) made of the chosen material, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.<br/>"
      <> "The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.<br/>"
      <> "<h3>Construct Spirit</h3>"
      <> "<i>Medium Construct</i><br/>"
      <> "<b>Armour Class:</b> 13 + the level of the spell (natural armour)<br/>"
      <> "<b>Hit Points:</b> 40 + 15 for each spell level above 4th<br/>"
      <> "<b>Speed:</b> 30 ft.<br/>"
      <> "<table><tr><td> STR </td><td> DEX </td><td> CON </td><td> INT </td><td> WIS </td><td> CHA </td></tr>"
      <> "<tr><td> 18 (+4) </td><td> 10 (+0) </td><td> 18 (+4) </td><td> 14 (+2) </td><td> 11 (+0) </td><td> 5 (-3) </td></tr></table>"
      <> "<b>Damage Resistances:</b> poison<br/>"
      <> "<b>Condition Immunities:</b> charmed, exhaustion, frightened, incapacitated, paralyzed, petrified, poisoned<br/>"
      <> "<b>Senses:</b> darkvision 60 ft., passive Perception 10<br/>"
      <> "<b>Languages:</b> understands the languages you speak<br/>"
      <> "<b>Challenge — Proficiency Bonus</b> equals your bonus<br/>"
      <> "<b><i>Heated Body (Metal Only).</i></b> A creature that touches the construct or hits it with a melee attack while within 5 feet of it takes 1d10 fire damage.<br/>"
      <> "<b><i>Stony Lethargy (Stone Only).</i></b> When a creature the construct can see starts its turn within 10 feet of the construct, the construct can force it to make a Wisdom saving throw against your spell save DC. On a failed save, the target can’t use reactions and its speed is halved until the start of its next turn.<br/>"
      <> "<h4>Actions</h4>"
      <> "<b><i>Multiattack.</i></b> The construct makes a number of attacks equal to half this spell's level (rounded down).<br/>"
      <> "<b><i>Slam.</i></b> Melee Weapon Attack: your spell attack modifier to hit, reach 5 ft., one target. Hit: 1d8 + 4 + the spell's level bludgeoning damage.<br/>"
      <> "<h4>Reactions</h4>"
      <> "<b><i>Berserk Lashing (Clay Only).</i></b> When the construct takes damage, it makes a slam attack against a random creature within 5 feet of it. If no creature is within reach, the construct moves up to half its speed toward an enemy it can see, without provoking opportunity attacks."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
  ]

fifthLevel :: [Spell]
fifthLevel =
  [ Spell
    { spName = "Animate Objects"
    , spLevel = Five
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Objects come to life at your command. Choose up to ten nonmagical objects within range that are not being worn or carried. Medium targets count as two objects, Large targets count as four objects, Huge targets count as eight objects. You can't animate any object larger than Huge. Each target animates and becomes a creature under your control until the spell ends or until reduced to 0 hit points.<br/>" <>
      "As a bonus action, you can mentally command any creature you made with this spell if the creature is within 500 feet of you (if you control multiple creatures, you can command any or all of them at the same time, issuing the same command to each one). You decide what action the creature will take and where it will move during its next turn, or you can issue a general command, such as to guard a particular chamber or corridor. If you issue no commands, the creature only defends itself against hostile creatures. Once given an order, the creature continues to follow it until its task is complete.<br/>" <>
      "<h3>Animated Object Statistics</h3>" <>
      "<table><tr><th> Size   </th><th> HP  </th><th> AC  </th><th> Str </th><th> Dex </th><th> Attack </th></tr>" <>
      "<tr><td> Tiny   </td><td> 20  </td><td> 18  </td><td> 4   </td><td> 18  </td><td> +8 to hit, 1d4+4 damage  </td></tr>" <>
      "<tr><td> Small  </td><td> 25  </td><td> 16  </td><td> 6   </td><td> 14  </td><td> +6 to hit, 1d8+2 damage  </td></tr>" <>
      "<tr><td> Medium </td><td> 40  </td><td> 13  </td><td> 10  </td><td> 12  </td><td> +5 to hit, 2d6+1 damage  </td></tr>" <>
      "<tr><td> Large  </td><td> 50  </td><td> 10  </td><td> 14  </td><td> 10  </td><td> +6 to hit, 2d10+2 damage </td></tr>" <>
      "<tr><td> Huge   </td><td> 80  </td><td> 10  </td><td> 18  </td><td> 6   </td><td> +8 to hit, 2d12+4 damage </td></tr></table>" <>
      "An animated object is a construct with AC, hit points, attacks, Strength, and Dexterity determined by its size. Its Constitution is 10 and its Intelligence and Wisdom are 3, and its Charisma is 1. Its speed is 30 feet; if the object lacks legs or other appendages it can use for locomotion, it instead has a flying speed of 30 feet and can hover. If the object is securely attached to a surface or a larger object, such as a chain bolted to a wall, its speed is 0. It has <i>blindsight</i> with a radius of 30 feet and is blind beyond that distance. When the animated object drops to 0 hit points, it reverts to its original object form, and any remaining damage carries over to its original object form.<br/>" <>
      "If you command an object to attack, it can make a single melee attack against a creature within 5 feet of it. It makes a slam attack with an attack bonus and bludgeoning damage determined by its size. The GM might rule that a specific object inflicts slashing or piercing damage based on its form."
    , spHigher = Just "If you cast this spell using a spell slot of 6th level or higher, you can animate two additional objects for each slot level above 5th."
    }
  , Spell
    { spName = "Bigbys Hand"
    , spLevel = Five
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (an eggshell and a snakeskin glove)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You create a Large hand of shimmering, translucent force in an unoccupied space that you can see within range. The hand lasts for the spell's duration, and it moves at your command, mimicking the movements of your own hand.<br/>" <>
      "The hand is an object that has AC 20 and hit points equal to your hit point maximum. If it drops to 0 hit points, the spell ends. It has a Strength of 26 (+8) and a Dexterity of 10 (+0). The hand doesn't fill its space.<br/>" <>
      "When you cast the spell and as a bonus action on your subsequent turns, you can move the hand up to 60 feet and then cause one of the following effects with it.<br/>" <>
      "<b><i>Clenched Fist.</i></b> The hand strikes one creature or object within 5 feet of it. Make a melee spell attack for the hand using your game statistics. On a hit, the target takes 4d8 force damage.<br/>" <>
      "<b><i>Forceful Hand.</i></b> The hand attempts to push a creature within 5 feet of it in a direction you choose. Make a check with the hand's Strength contested by the Strength (<i>Athletics</i>) check of the target. If the target is Medium or smaller, you have advantage on the check. If you succeed, the hand pushes the target up to 5 feet plus a number of feet equal to five times your spellcasting ability modifier. The hand moves with the target to remain within 5 feet of it.<br/>" <>
      "<b><i>Grasping Hand.</i></b> The hand attempts to grapple a Huge or smaller creature within 5 feet of it. You use the hand's Strength score to resolve the grapple. If the target is Medium or smaller, you have advantage on the check. While the hand is grappling the target, you can use a bonus action to have the hand crush it. When you do so, the target takes bludgeoning damage equal to 2d6 + your spellcasting ability modifier.<br/>" <>
      "<b><i>Interposing Hand.</i></b> The hand interposes itself between you and a creature you choose until you give the hand a different command. The hand moves to stay between you and the target, providing you with half cover against the target. The target can't move through the hand's space if its Strength score is less than or equal to the hand's Strength score. If its Strength score is higher than the hand's Strength score, the target can move toward you through the hand's space, but that space is difficult terrain for the target."
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the damage from the clenched fist option increases by 2d8 and the damage from the grasping hand increases by 2d6 for each slot level above 5th."
    }
  , Spell
    { spName = "Cone of Cold"
    , spLevel = Five
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a small crystal or glass cone)"
    , spDuration = "Instantaneous"
    , spDescription = "A blast of cold air erupts from your hands. Each creature in a 60-foot cone must make a Constitution saving throw. A creature takes 8d8 cold damage on a failed save, or half as much damage on a successful one.<br/>" <>
      "A creature killed by this spell becomes a frozen statue until it thaws."
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the damage increases by 1d8 for each slot level above 5th."
    }
  , Spell
    { spName = "Conjure Elemental"
    , spLevel = Five
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "90 feet"
    , spComponents = "V,S,M (burning incense for air, soft clay for earth, sulphur and phosphorous for fire, or water and sand for water)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You call forth an elemental servant. Choose an area of air, earth, fire, or water that fills a 10-foot cube within range. An elemental of challenge rating 5 or lower appropriate to the area you chose appears in an unoccupied space within 10 feet of it. For example, a fire elemental emerges from a bonfire, and an earth elemental rises up from the ground. The elemental disappears when it drops to 0 hit points or when the spell ends.<br/>" <>
      "The elemental is friendly to you and your companions for the duration. Roll initiative for the elemental, which has its own turns. It obeys any verbal commands that you issue to it (no action required by you). If you don't issue any commands to the elemental, it defends itself from hostile creatures but otherwise takes no actions.<br/>" <>
      "If your concentration is broken, the elemental doesn't disappear. Instead, you lose control of the elemental, it becomes hostile toward you and your companions, and it might attack. An uncontrolled elemental can't be dismissed by you, and it disappears 1 hour after you summoned it.<br/>" <>
      "The GM has the elemental's statistics. Sample elementals can be found below.<br/>" <>
      "<h3>Sample Elementals</h3>" <>
      "<table><tr><th> CR  </th><th> Creature Name </th></tr>" <>
      "<tr><td> 1/4 </td><td> Steam Mephit  </td></tr>" <>
      "<tr><td> 1/2 </td><td> Dust Mephit, Ice Mephit, Magma Mephit, Magmin </td></tr>" <>
      "<tr><td> 2   </td><td> Azer, Gargoyle  </td></tr>" <>
      "<tr><td> 5   </td><td> Air Elemental, Earth Elemental, Fire Elemental, Salamander, Water Elemental, Xorn </td></tr>" <>
      "<tr><td> 6   </td><td> Invisible Stalker  </td></tr></table>"
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the challenge rating increases by 1 for each slot level above 5th."
    }
  , Spell
    { spName = "Creation"
    , spLevel = Five
    , spType = Illusion
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a tiny piece of matter of the same type of the item you plan to create)"
    , spDuration = "Special"
    , spDescription = "You pull wisps of shadow material from the Shadowfell to create a nonliving object of vegetable matter within range:  soft goods, rope, wood, or something similar. You can also use this spell to create mineral objects such as stone, crystal, or metal. The object created must be no larger than a 5-foot cube, and the object must be of a form and material that you have seen before.<br/>"
      <> "The duration depends on the object's material. If the object is composed of multiple materials, use the shortest duration.<br/>"
      <> "<table><tr><th> Material              </th><th> Duration </th></tr>"
      <> "<tr><td> Vegetable matter      </td><td> 1 day    </td></tr>"
      <> "<tr><td> Stone or crystal      </td><td> 1 day    </td></tr>"
      <> "<tr><td> Precious metals       </td><td> 1 day    </td></tr>"
      <> "<tr><td> Gems                  </td><td> 1 day    </td></tr>"
      <> "<tr><td> Adamantine or mithral </td><td> 1 day    </td></tr></table>"
      <> "Using any material created by this spell as another spell's material component causes that spell to fail."
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the cube increases by 5 feet for each slot level above 5th."
    }
  , Spell
    { spName = "Greater Restoration"
    , spLevel = Five
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (diamond dust worth at least 100 gp, which the spell consumes)"
    , spDuration = "Instantaneous"
    , spDescription = "You imbue a creature you touch with positive energy to undo a debilitating effect. You can reduce the target's exhaustion level by one, or end one of the following effects on the target:<br/>"
      <> "- One effect that charmed or petrified the target\n"
      <> "- One curse, including the target's attunement to a cursed magic item\n"
      <> "- Any reduction to one of the target's ability scores\n"
      <> "- One effect reducing the target's hit point maximum"
    , spHigher = Nothing
    }
  , Spell
    { spName = "Passwall"
    , spLevel = Five
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a pinch of sesame seeds)"
    , spDuration = "1 hour"
    , spDescription = "A passage appears at a point of your choice that you can see on a wooden, plaster, or stone surface (such as a wall, a ceiling, or a floor) within range, and lasts for the duration. You choose the opening's dimensions: up to 5 feet wide, 8 feet tall, and 20 feet deep. The passage creates no instability in a structure surrounding it.<br/>"
      <> "When the opening disappears, any creatures or objects still in the passage created by the spell are safely ejected to an unoccupied space nearest to the surface on which you cast the spell."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Skill Empowerment"
    , spLevel = Five
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "Your magic deepens a creatures understanding of its own talent. You touch one willing creature and give it expertise in one skill of your choice; until the spell ends, the creature doubles its proficiency bonus for ability checks it makes that use the chosen skill.<br/>"
      <> "You must choose a skill in which the target is proficient and that isnt already benefiting from an effect, such as Expertise, that doubles its proficiency bonus."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Transmute Rock"
    , spLevel = Five
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (clay and water)"
    , spDuration = "Until Dispelled"
    , spDescription = "You choose an area of stone or mud that you can see that fits within a 40-foot cube and is within range, and choose one of the following effects.<br/>"
      <> "<b><i>Transmute Rock to Mud.</i></b> Nonmagical rock of any sort in the area becomes an equal volume of thick, flowing mud that remains for the spell's duration.<br/>"
      <> "The ground in the spell's area becomes muddy enough that creatures can sink into it. Each foot that a creature moves through the mud costs 4 feet of movement, and any creature on the ground when you cast the spell must make a Strength saving throw. A creature must also make the saving throw when it moves into the area for the first time on a turn or ends its turn there. On a failed save, a creature sinks into the mud and is restrained, though it can use an action to end the restrained condition on itself by pulling itself free of the mud.<br/>"
      <> "If you cast the spell on a ceiling, the mud falls. Any creature under the mud when it falls must make a Dexterity saving throw. A creature takes 4d8 bludgeoning damage on a failed save, or half as much damage on a successful one.<br/>"
      <> "<b><i>Transmute Mud to Rock.</i></b> Nonmagical mud or quicksand in the area no more than 10 feet deep transforms into soft stone for the spell's duration. Any creature in the mud when it transforms must make a Dexterity saving throw. On a successful save, a creature is shunted safely to the surface in an unoccupied space. On a failed save, a creature becomes restrained by the rock. A restrained creature, or another creature within reach, can use an action to try to break the rock by succeeding on a DC 20 Strength check or by dealing damage to it. The rock has AC 15 and 25 hit points, and it is immune to poison and psychic damage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Wall of Force"
    , spLevel = Five
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a pinch of powder made by crushing a clear gemstone)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "An invisible wall of force springs into existence at a point you choose within range. The wall appears in any orientation you choose, as a horizontal or vertical barrier or at an angle. It can be free floating or resting on a solid surface. You can form it into a hemispherical dome or a sphere with a radius of up to 10 feet, or you can shape a flat surface made up of ten 10-foot-by-10-foot panels. Each panel must be contiguous with another panel. In any form, the wall is 1/4 inch thick. It lasts for the duration. If the wall cuts through a creature's space when it appears, the creature is pushed to one side of the wall (your choice which side).<br/>"
      <> "Nothing can physically pass through the wall. It is immune to all damage and can't be dispelled by dispel magic. A disintegrate spell destroys the wall instantly, however. The wall also extends into the Ethereal Plane, blocking ethereal travel through the wall."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Wall of Stone"
    , spLevel = Five
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a small block of granite)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "A nonmagical wall of solid stone springs into existence at a point you choose within range. The wall is 6 inches thick and is composed of ten 10-foot- by-10-foot panels. Each panel must be contiguous with at least one other panel. Alternatively, you can create 10-foot-by-20-foot panels that are only 3 inches thick.<br/>"
      <> "If the wall cuts through a creature's space when it appears, the creature is pushed to one side of the wall (your choice). If a creature would be surrounded on all sides by the wall (or the wall and another solid surface), that creature can make a Dexterity saving throw. On a success, it can use its reaction to move up to its speed so that it is no longer enclosed by the wall.<br/>"
      <> "The wall can have any shape you desire, though it can't occupy the same space as a creature or object. The wall doesn't need to be vertical or rest on any firm foundation. It must, however, merge with and be solidly supported by existing stone. Thus, you can use this spell to bridge a chasm or create a ramp.<br/>"
      <> "If you create a span greater than 20 feet in length, you must halve the size of each panel to create supports. You can crudely shape the wall to create crenellations, battlements, and so on.<br/>"
      <> "The wall is an object made of stone that can be damaged and thus breached. Each panel has AC 15 and 30 hit points per inch of thickness. Reducing a panel to 0 hit points destroys it and might cause connected panels to collapse at the GM's discretion.<br/>"
      <> "If you maintain your concentration on this spell for its whole duration, the wall becomes permanent and can't be dispelled. Otherwise, the wall disappears when the spell ends."
    , spHigher = Nothing
    }
  ]