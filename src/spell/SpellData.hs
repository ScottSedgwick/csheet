{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module SpellData where

import Data.Map (Map, fromList)
import Data.Text.Lazy (Text)

data Spell = Spell
  { spName :: Text
  , spLevel :: Int
  , spType :: SpellType
  , spRitual :: Bool
  , spTime :: Text
  , spRange :: Text
  , spComponents :: Text
  , spDuration :: Text
  , spDescription :: Text
  , spHigher :: Maybe Text
  } deriving stock Show

data SpellType 
  = Abjuration
  | Conjuration
  | Divination
  | Enchantment
  | Evocation
  | Illusion
  | Necromancy
  | Transmutation
  deriving stock (Show, Eq)

spellMap :: Map Text Spell
spellMap = fromList (map (\s -> (spName s, s)) spells)

spells :: [Spell]
spells = 
  [
  -- Cantrips
    Spell 
    { spName = "Acid Splash"
    , spLevel = 0
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You hurl a bubble of acid. Choose one creature within range, or choose two creatures within range that are within 5 feet of each other. A target must succeed on a Dexterity saving throw or take 1d6 acid damage.\\\\This spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    , spHigher = Nothing
    }
  , Spell 
    { spName = "Blade Ward"
    , spLevel = 0
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 round"
    , spHigher = Nothing
    , spDescription = "You extend your hand a trace a sigil of warding in the air. Until the end of your next turn, you have resistance against bludgeoning, piercing and slashing damage dealt by weapon attacks."}
  , Spell 
    { spName = "Booming Blade (Sword Coast Adventurers Guide)"
    , spLevel = 0
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
    , spLevel = 0
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
    , spLevel = 0
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You choose nonmagical flame that you can see within range and that fits within a 5-foot cube. You affect it in one of the following ways:\\begin{itemize}\\item You instantaneously expand the flame 5 feet in one direction, provided that wood or other fuel is present in the new location.\\item You instantaneously extinguish the flames within the cube.\\item You double or halve the area of bright light and dim light cast by the flame, change its color, or both. The change lasts for 1 hour.\\item You cause simple shapes - such as the vague form of a creature, an inanimate object, or a location - to appear within the flames and animate as you like. The shapes last for 1 hour.\\end{itemize}If you cast this spell multiple times, you can have up to three of its non-instantaneous effects active at a time, and you can dismiss such an effect as an action."
    }
  , Spell 
    { spName = "Create Bonfire"
    , spLevel = 0
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spHigher = Nothing
    , spDescription = "You create a bonfire on ground that you can see within range. Until the spell ends, the magic bonfire fills a 5-foot cube. Any creature in the bonfire’s space when you cast the spell must succeed on a Dexterity saving throw or take 1d8 fire damage. A creature must also make the saving throw when it moves into the bonfire’s space for the first time on a turn or ends its turn there.\\\\The bonfire ignites flammable objects in its area that aren’t being worn or carried.\\\\The spell’s damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
  , Spell 
    { spName = "Dancing Lights"
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a bit of phosphorous or wychwood, or a glowworm)"
    , spDuration = "Concentration, up to 1 minute"
    , spHigher = Nothing
    , spDescription = "You create up to four torch-sized lights within range, making them appear as torches, lanterns or glowing orbs that hover in the air for the duration. You can also combine the four lights into one glowing vaguely humanoid form of medium size. Whichever form you choose, each light sheds dim light in a 10-foot radius.\\\\As a bonus action on your turn, you can move the lights up to 60 feet to a new spot within range. A light must be within 20 feet of another light created by this spell, and a light winks out if it exceeds the spell's range."
    }
  , Spell 
    { spName = "Encode Thoughts (Guildmasters Guide to Ravnica)"
    , spLevel = 0
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
    { spName = "Fire Bolt"
    , spLevel = 0
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
    , spLevel = 0
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
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You cause numbing frost to form on one creature that you can see within range. The target must make a Constitution saving throw. On a failed save, the target takes 1d6 cold damage, and it has disadvantage on the next weapon attack roll it makes before the end of its next turn.\\\\The spell’s damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
  , Spell 
    { spName = "Green Flamed Blade (Sword Coast Adventurers Guide)"
    , spLevel = 0
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
    { spName = "Gust"
    , spLevel = 0
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
    , spLevel = 0
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a living flea)"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You cause a cloud of mites, fleas, and other parasites to appear momentarily on one creature you can see within range. The target must succeed on a Constitution saving throw, or it takes 1d6 poison damage and moves 5 feet in a random direction if it can move and its speed is at least 5 feet. Roll a d4 for the direction: 1, north; 2: south; 3, east; or 4, west. This movement doesn't provoke opportunity attacks, and if the direction rolled is blocked, the target doesn't move.\\\\The spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6) and 17th level (4d6)."
    }
  , Spell 
    { spName = "Light"
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,M (a firefly or phosphoresecent moss)"
    , spDuration = "1 hour"
    , spHigher = Nothing
    , spDescription = "You touch one object that is not larger than 10 feet in any dimension. Until the spell ends, the object sheds bright light in a 20-foot radius and dim light for an additional 20 feet. The light can be colored as you like. Completely covering the object with something opaque blocks the light. The spell ends if you cast it again or dismiss it as an action.\\\\If you target an object worn or held by a hostile creature, that creature must succeed on a Dexterity saving throw to avoid the spell."
    }
  , Spell 
    { spName = "Lightning Lure (Sword Coast Adventurers Guide)"
    , spLevel = 0
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
    , spLevel = 0
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spDescription = "A spectral, floating hand appears at a point you choose within range. The hand lasts for the duration or until you dismiss it as an action. The hand vanishes if it is ever more than 30 feet away from you or if you cast this spell again.\\\\You can use your action to control the hand. You can use the hand to manipulate an object, open an unlocked door or container, stow or retrieve an item from an open contrainer, or pour the contents out of a vial. You can move the hand up to 30 feet each time you use it.\\\\You can't attack, activate magic items, or carry more than 10 pounds."
    , spHigher = Nothing
    }
  , Spell 
    { spName = "Mending"
    , spLevel = 0
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "V,S,M (two lodestones)"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "This spell repairs a single break or tear in an object you touch, such as a broken chain link, two halves of a broken key, a torn cloak, or a leaking wineskin. As long as the break or tear is no larger than 1 foot in any dimension, you mend it, leaving no trace of former damage.\\\\This spell can physically repair a magic item or construct, but the spell can't restore magic to such an object."
    }
  , Spell 
    { spName = "Message"
    , spLevel = 0
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a short piece of copper wire)"
    , spDuration = "1 round"
    , spHigher = Nothing
    , spDescription = "You point your finger toward a creature within range and whisper a message. The target (and only the target) hears the message and can reply in a whisper that only you can hear.\\\\You can cast this spell through solid objects if you are familiar with the target and know it is beyond the barrier. Magical silence, 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood blocks the spell. The spell doesn't have to follow a straight line and can freely travel around corners or through openings."
    }
  , Spell 
    { spName = "Mind Sliver (Unearthed Arcana)"
    , spLevel = 0
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "1 round"
    , spHigher = Nothing
    , spDescription = "You drive a disorienting spike of psychic energy into the mind of one creature you can see within range. The target must make an Intelligence saving throw. Unless the saving throw is successful, the target takes 1d6 psychic damage, and the first time it makes a saving throw before the end of your next turn, it must roll a d4 and subtract the number rolled from the save.\\\\This spell’s damage increases by 1d6 when you reach certain levels: 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
    }
  , Spell 
    { spName = "Minor Illusion"
    , spLevel = 0
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "S,M (a bit of fleece)"
    , spDuration = "1 minute"
    , spHigher = Nothing
    , spDescription = "You create a sound or image of an object within range that lasts for the duration. The illusion also ends if you dismiss it as an action or cast this spell again.\\\\If you create a sound, its volume can range from a whisper to a scream. It can be your voice, someone else's voice, a lion's roar, a beating of drums, or any other sound you choose. The sound continues unabated throughout the duration, or you can make discrete sounds at different times before the spell ends.\\\\If you create an image of an object - such as a chair, muddy footprints or a small chest - it must be no larger than a 5-foot cube. The image can't create sound, light, smell, or any other sensory effect. Physical interaction with the image reveals it to be an illusion, because things can pass through it.\\\\If a creature uses its action to examine the sound or image, the creature can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a create discerns the illusion for what it is, the illusion becomes faint to the creature."
    }
  , Spell 
    { spName = "Mold Earth"
    , spLevel = 0
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
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "10 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You extend your hand toward a creature you can see within range and prject a puff of noxious gas from your palm. The creature must succeed on a Constitution saving throw or take 1d12 poison damage.\\\\This spell's damage increases by 1d12 when you reach 5th level (2d12), 11th level (3d12), and 17th level (4d12)."
    }
  , Spell 
    { spName = "Prestidigitation"
    , spLevel = 0
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
    { spName = "Ray of Frost"
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "A frigid beam of blue-white light streaks toward a creature within range. Make a ranged spell attack against the target. On a hit, it takes 1d8 cold damage, and its speed is reduced by 10 feet until the start of your next turn.\\\\The spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8) and 17th level (4d8)."
    }
  , Spell 
    { spName = "Shape Water"
    , spLevel = 0
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
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "Lightning springs from your hand to deliver a shock to a creature you try to touch. Make a melee spell attack against the target. You have advantage on the attack roll if the target is wearing armour made of metal. On a hit, the target takes 1d8 lightning damage, and can't take reactions until the start of its next turn.\\\\This spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
  , Spell 
    { spName = "Sword Burst (Sword Coast Adventurers Guide)"
    , spLevel = 0
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
    , spLevel = 0
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
    , spLevel = 0
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "You point at one creature you can see within range, and the sound of a dolorous bell fills the air around it for a moment. The target must succeed on a Wisdom saving throw or take 1d8 necrotic damage. If the target is missing any of its hit points, it instead takes 1d12 necrotic damage.\\\\The spell's damage increases by one die when you reach 5th level (2d8 or 2d12), 11th level (3d8 or 3d12) and 17th level (4d8 or 4d12)."
    }
  , Spell 
    { spName = "True Strike"
    , spLevel = 0
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
    , spLevel = 0
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spHigher = Just "This spell's damage increases by 1d4 when you reach 5th level (2d4), 11th level (3d4) and 17th level (4d4)."
    , spDescription = "You unleash a string of insults laced with subtle enchantments at a creature you can see within range. If the target can hear you (though it need not understand you), it must succeed on a Wisdom saving throw or take 1d4 psychic damage and have disadvantage on the next attack roll it makes before the end of it's next turn."
    }
  -- 1st Level Spells
  , Spell
    { spName = "Absorb Elements"
    , spLevel = 1
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
    , spLevel = 1
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
    , spLevel = 1
    , spType = Abjuration
    , spRitual = True
    , spTime = "1 minute"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a tiny bell and a piece of fine silver wire)"
    , spDuration = "8 hours"
    , spDescription = "You set an alarm against unwanted intrusion. Choose a door, window, or an area within range that is no larger than a 20-foot cube. Until the spell ends, an alarm alerts you whenever a Tiny or larger creature touches or enters the warded area. When you cast the spell, you can designate creatures that won't set off the alarm. You also choose whether the alarm is mental or audible.\\\\A mental alarm alerts you with a ping in your mind if you are within 1 mile of the warded area. This ping awakens you if you are sleeping.\\\\An audible alarm produces the sound of a hand bell for 10 seconds within 60 feet."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Burning Hands"
    , spLevel = 1
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "15-foot cone"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "As you hold your hands with thumbs touching and fingers spread, a thin sheet of flames shoots forth from your outstretched fingertips. Each creature in a 15-foot cone must make a Dexterity saving throw. A creature takes 3d6 fire damage on a failed save, or half as much damage on a successful one.\\\\The fire ignites any flammable objects in the area that aren't being worn or carried"
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d6 for each slot level above 1st."
    }
  , Spell
    { spName = "Catapult"
    , spLevel = 1
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
    , spLevel = 1
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
    { spName = "Charm Person"
    , spLevel = 1
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
    , spLevel = 1
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
    , spLevel = 1
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self (15-foot cone)"
    , spComponents = "V,S,M (a pinch of powder or sand that is coloured red, yellow and blue)"
    , spDuration = "1 round"
    , spDescription = "A dazzling array of flashing, coloured light springs from your hand. Roll 6d10; the total is how many hit points of creatures this spell can affect. Creatures in a 15-foot cone originating from you are affected in ascending order of their current hit points (ignoring unconscious creatures and creatures that can't see).\\\\Starting with the creature that has the lowest current hit points, each creature affected by this spell is blinded until the spell ends. Subtract each creature's hit points from the total before moving to the creature with the next lowest hit points. A creature's hit points must be equal to or less that the remaining total for that creature to be affected."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2d10 for each slot level above 1st."
    }
  , Spell
    { spName = "Comprehend Languages"
    , spLevel = 1
    , spType = Divination
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a pinch of soot and salt)"
    , spDuration = "1 hour"
    , spDescription = "For the duration, you understand the literal meaning of any spoken language that you hear. You also understand any written language that you see, but you must be touching the surface on which the words are written. It takes about 1 minute to read one page of text.\\\\This spell doesn't decode secret messages in a text or a glyph, such as an arcane sigil, that isn't part of a written langauage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Detect Magic"
    , spLevel = 1
    , spType = Divination
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "For the duration, you sense the presence of magic within 30 feet of you. If you sense magic in this way, you can use your action to see a faint aura around any visible creature or object in the area that bears magic, and you learn its school of magic, if any.\\\\The spell can penetrate most barriers, but it is blocked by 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood or dirt."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Disguise Self"
    , spLevel = 1
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 hour"
    , spDescription = "You make yourself - including your clothing, armour, weapons and other belongings on your person - look different until the spell ends or you use your action to dismiss it. You can seem 1 foot shorter or taller and can appear thin, fat or in between. You can't change your body type, so you must adopt a form that has the same basic arrangement of limbs. Otherwise, the extent of the illusion is up to you.\\\\The changes wrought by this spell fail to hold up to physical inspection. For example, if you use this spell to add a hat to your outfit, objects pass through the hat, and anyone who touches it would feel nothing or would feel your hear and hair. If you use this spell to appear thinner than you are, the hand of someone who reaches out to touch you would bump into you while it was seemingly still in midair.\\\\To discern that you are disguised, a creature can use its action to inspect your appearance and must succeed on an Intelligence (Investigation) check against your spell save DC."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Dissonant Whispers"
    , spLevel = 1
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
    , spLevel = 1
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
    , spLevel = 1
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
    { spName = "False Life"
    , spLevel = 1
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
    , spLevel = 1
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
    , spLevel = 1
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 hour"
    , spRange = "10 feet"
    , spComponents = "V,S,M (10gp worth of charcoal, incense, and herbs that must be consumed by fire in an brass brazier)"
    , spDuration = "Instantaneous"
    , spDescription = "You gain the service of a familiar, a spirit that takes an animal form you choose: bat, cat crab, frog (toad), hawk, lizard, octopus, owl, poisonous snake, fish (quipper), rat, raven, sea horse, spider or weasel. Appearing in an unoccupied space within range, the familiar has the statistics of the chosen form, though it is a celestial, fey or fiend (your choice) instead of a beast.\\\\Your familiar acts independently of you, but it always obeys your commands. In combat, it rools its own initiative and acts on its own turn. A familiar can't attack, but it can take other actions as normal.\\\\When the familiar drops to 0 hit points, it disappears, leaving behind no physical form. It reappears after you cast this spell again.\\\\While your familiar is within 100 feet of you, you can communicate with it telepathically. Additionally, as an action, you can see through your familiar's eyes and hear what it hears until the start of your next turn, gaining the benefits of any special senses that the familiar has. During this time, you are deaf and blind with regard to your own senses.\\\\As an action, you can temprarily dismiss your familiar. It disappears into a pocket dimension where it awaits your summons. Alternatively, you can dismiss it forever. As an action while it is temporarily dismissed, you can cause it to appear in any unoccupied space within 30 feet of you.\\\\You can't have more than one familiar at a time. If you cast this spell while you already have a familiar, you instead cause it to adopt a new form. Choose one of the forms from the above list. Your familiar transforms into the chosen creature.\\\\Finally, when you cast a spell with a range of touch, your familiar can deliver the spell as if it had cast the spell. Your familiar must be within 100 feet of you, and it must use its reaction to deliver the spell when you cast it. If the spell requires an attack roll, you use your attack modifier for the roll."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Fog Cloud"
    , spLevel = 1
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
    , spLevel = 1
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of pork rind or butter)"
    , spDuration = "1 minute"
    , spDescription = "Slick grease covers the ground in a 10-foot square centered on a point within range and turns it into difficult terrain for the duration.\\\\When the grease appears, each creature standing in its area must succeed on a Dexterity saving throw or fall prone. A creature that enters the area or ends its turn there must also succeed on a Dexterity saving throw or fall prone."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Healing Word"
    , spLevel = 1
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
    , spLevel = 1
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
    { spName = "Ice Knife"
    , spLevel = 1
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
    , spLevel = 1
    , spType = Divination
    , spRitual = True
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "V,S,M (a pearl worth 100gp and an owl feather)"
    , spDuration = "Instantaneous"
    , spDescription = "You choose one object that you must touch throughout the casting of the spell. If it is a magic item or some other magic-imbued object, you learn its properties and how to use them, whether it requires attunement to use, and how many charges it has, if any. You learn whether any spells are affecting the item and what they are. If the item was created by a spell, you learn which spell created it.\\\\If you instead touch a creature throughout the casting, you learn what spells, if any, are currently affecting it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Illusory Script"
    , spLevel = 1
    , spType = Illusion
    , spRitual = True
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "S,M (a lead-based ink worth at least 10gp which the spell consumes)"
    , spDuration = "10 days"
    , spDescription = "You write on parchment, paper or some other suitable writing material and imbue it with a potent illusion that lasts for the duration.\\\\To you and any creatures you designate when you cast the spell, the writing appears normal, written in your hand, and conveys whatever meaning you intended when you wrote the text. To all others, the writing appears as if it were in an unknown or magical script that is unintelligible. Alternatively, you can cause the writing to appear to be an entirely different message, written in a different hand and language, thought the language must be one that you know.\\\\Should the spell be dispelled, the original script and the illusion both disappear.\\\\A creature with truesight can read the hidden message."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Jump"
    , spLevel = 1
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
    , spLevel = 1
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
    { spName = "Mage Armour"
    , spLevel = 1
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
    , spLevel = 1
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
    , spLevel = 1
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (holy water or powdered silver and iron, which the spell consumes)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "Until the spell ends, one willing creature you touch is protected against certain types of creatures: abberations, celestials, elementals, fey, fiends and undead.\\\\The protection grants several benefits. Creatures of those types have disadvantage on attack rools against the target. The target also can't be charmed, frightened, or possessed by them. If the target is already charmed, frightened, or possessed by such a creature, the target has advantage on any new saving throw against the relevant effect."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Ray of Sickness"
    , spLevel = 1
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
    { spName = "Shield"
    , spLevel = 1
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
    { spName = "Silent Image"
    , spLevel = 1
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of fleece)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "You create the image of an object, a creature, or some other visible phenomenon that is no larger than a 15-foot cube. The image appears at a spot within range and lasts for the duration. The image is purely visual; it isn't accompanied by sounds, smell, or other sensory effects.\\\\You can use your action to cause the image to move to any spot within range. As the image changes location, you can alter its appearance so that its movements appear natural for the image. For example, if you create an image of a creature and move it, you can alter the image so that it appears to be walking.\\\\Physical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Sleep"
    , spLevel = 1
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (a pinch of fine sand, rose petals, or a cricket)"
    , spDuration = "1 minute"
    , spDescription = "This spell sends creatures into a magical slumber. Roll 5d8: the total is how many hit points of creatures this spell can affect. Creatures within 20 feet of a point you choose within range are affected in ascending order of their current hit points (ignoring unconscious creatures).\\\\Starting with the creature that has the lowest current hit points, each creature affected by this spell falls unconscious until the spell ends, the sleeper takes damage, or someone uses an action to shake or slap the sleeper awake. Subtract each creatures's hit points from the total before moving on to the creature with the next lowest hit points. A creature's hit points must be equal to or less than the remaining total for that creature to be affected.\\\\Undead and creatures immune to being charmed aren't affected by this spell."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2d8 for each slot level above 1st."
    }
  , Spell
    { spName = "Snare"
    , spLevel = 1
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "S,M (25 feet of rope, which the spell consumes)"
    , spDuration = "8 hours"
    , spDescription = "As you cast this spell, you use the rope to create a circle with a 5-foot radius on the ground or the floor. When you finish casting, the rope disappears and the circle becomes a magic trap.\\\\This trap is nearly invisible, requiring a successful Intelligence (Investigation) check against your spell save DC to be discerned.\\\\The trap triggers when a Small, Medium, or Large creature moves onto the ground or the floor in the spell's radius. That creature must succeed on a Dexterity saving throw or be magically hoisted into the air, leaving it hanging upside down 3 feet above the ground or floor. The creature is restrained there until the spell ends.\\\\A restrained creature can make a Dexterity saving throw at the end of each of its turns, ending the effect on iteself on a success. Alternatively, the creature or someone else who can reach it can use an action to make an Intelligence (Arcana) check against your spell save DC. On a success, the restrained effect ends.\\\\After the trap is triggered, the spell ends when no creature is restrained by it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Tasha's Hideous Laughter"
    , spLevel = 1
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (tiny tarts and a feather that is waved in the air)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A creature of your choice that you can see within range perceives everything as hilariously funny and falls into fits of laughter if this spell affects it. The target must succeed on a Wisdom saving throw or fall prone, becoming incapacitated and unable to stand up for the duration. A creature with an intelligence score of 4 or less isn't affected.\\\\At the end of each of its turns, and each time it takes damage, the target can make another Wisdom saving throw. The target has advantage on the saving throw if it's triggered by damage. On success, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Tenser's Floating Disk"
    , spLevel = 1
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a drop of mercury)"
    , spDuration = "1 hour"
    , spDescription = "This spell creates a circular, horizontal plane of force, 3 feet in diameter and 1 inch thick, that floats 3 feet above groud in an unoccupiied space of your choice that you can see within range. The disk remains for the duration, and can hold up to 500 pounds. If more weight is placed on it, the spell ends, and everything on the disk falls to the ground.\\\\The disk is immobile while you are within 20 feet of it. If you move more than 20 feet away from it, the disk follows you so that it remains within 20 feet of you. It can move across uneven terrain, up or down stairs, slopes and the like, but it can't cross an elevation change of 10 feet or more. For example, the disk can't move across a 10-foot-deep pit, nor could it leave such a pit if it was created at the bottom.\\\\If you move more than 100 feet from the disk (typically because it can't move around an obstacle to follow you), the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Thunderwave"
    , spLevel = 1
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self (15-foot cube)"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "A wave of thunderous force sweeps out from you. Each creature in a 15-foot cube originating from you must make a Constitution saving throw. On a failed save, a creature takes 2d8 thunder damage and is pushed 10 feet away from you. On a successful save, the creature takes half as much damage and isn't pushed.\\\\In addition, unsecured objects that are completely within the area of effect are automatically pushed 10 feet away from you by the spell's effect, and the spell emits a thunderous boom audible out to 300 feet."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the damage increases by 1d8 for each slot level above 1st."
    }
  , Spell
    { spName = "Unseen Servant"
    , spLevel = 1
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a piece of string and a bit of wood)"
    , spDuration = "1 hour"
    , spDescription = "This spell creates an invisible, mindless, shapeless force that performs simple tasks at your command until the spell ends. The servant springs into existence in an unoccupied space on the ground within range. It has AC 10, 1 hit point, and a Strength of 2, and it can't attack. If it drops to 0 hit points, the spell ends.\\\\Once on each of your turns as a bonus action, you can mentally command the servant to move up to 15 feet and interact with an object. The servant can perform simple tasks that a human servant would do, such as fetching things, cleaning, mending, folding clothes, lighting fires, serving food, and pouring wine. Once you give the command, the servant performs the task to the best of its ability until it completes the task, then waits for your next command.\\\\If you command the servant to perform a task that would move it more than 60 feet away from you, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Witch Bolt"
    , spLevel = 1
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a twig from a tree that has been struck by lightning)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A beam of crackling, blue energy lances out toward a creature within range, forming a sustained arc of lightning between you and the target. Make a ranged spell attack against that creature. On a hit, the target takes 1d12 lightning damage, and on each of your turns for the duration, you can use your action to deal 1d12 lightning damage to the target automatically. The spell ends if you use your action to do anything else. The spell also ends if the target is ever outside the spell's range or if it has total cover from you."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, the initial damage increases by 1d12 for each slot level above 1st."
    }
  -- 2nd Level Spells
  , Spell
    { spName = "Acid Arrow"
    , spLevel = 2
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
    , spLevel = 2
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
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 hour (concentration)"
    , spDescription = "You assume a different form. When you cast the spell, choose one of the following options, the effects of which last for the duration of the spell. While the spell lasts, you can end one option as an action to gain the benefits of a different one.\\\\ \\textbf{Aquatic Adaptation.} You adapt your body to an aquatic environment, sprouting gills and growing webbing between your fingers. You can breathe underwater and gain a swimming speed equal to your walking speed.\\\\ \\textbf{Change Appearance.} You transform your appearance. You decide what you look like, including your height, weight, facial features, sound of your voice, hair length, coloration, and distinguishing characteristics, if any. You can make yourself appear as a member of another race, though none of your statistics change. You also can't appear as a creature of a different size than you, and your basic shape stays the same; if you're bipedal, you can't use this spell to become quadrupedal, for instance. At any time for the duration of the spell, you can use your action to change your appearance in this way again.\\\\ \\textbf{Natural Weapons.} You grow claws, fangs, spines, horns, or a different natural weapon of your choice. Your unarmed strikes deal 1d6 bludgeoning, piercing, or slashing damage, as appropriate to the natural weapon you chose, and you are proficient with your unarmed strikes. Finally, the natural weapon is magic and you have a +1 bonus to the attack and damage rolls you make using it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Arcane Lock"
    , spLevel = 2
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (gold dust worth at least 25gp, which the spell consumes)"
    , spDuration = "Until dispelled"
    , spDescription = "You touch a closed door, window, gate, chest, or other entryway, and it becomes locked for the duration. You and the creatures you designate when you cast this spell can open the object normally. You can also set a password that, when spoken within 5 feet of the object, suppresses this spell for 1 minute. Otherwise, it is impassable until it is broken or the spell is dispelled or suppressed. Casting \\textit{knock} on the object suppresses \\textit{arcane lock} for 10 minutes. \\\\While affected by this spell, the object is more difficult to break or force open; the DC to break it or pick any locks on it increases by 10."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Arcanist's Magic Aura"
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a small square of silk)"
    , spDuration = "24 Hours"
    , spDescription = "You place an illusion on a creature or an object you touch so that divination spells reveal false information about it. The target can be a willing creature or an object that isn't being carried or worn by another creature.\\\\When you cast the spell, choose one or both of the following effects. The effect lasts for the duration. If you cast this spell on the same creature or object every day for 30 days, placing the same effect on it each time, the illusion lasts until it is dispelled.\\\\\\textbf{False Aura.} You change the way the target appears to spells and magical effects, such as \\textit{detect magic}, that detect magical auras. You can make a nonmagical object appear magical, a magical object appear nonmagical, or change the object's magical aura so that it appears to belong to a specific school of magic that you choose. When you use this effect on an object, you can make the false magic apparent to any creature that handles the item.\\\\\\textbf{Mask.} You change the way the target appears to spells and magical effects that detect creature types, such as a paladin's Divine Sense or the trigger of a \\textit{symbol} spell. You choose a creature type and other spells and magical effects treat the target as if it were a creature of that type or of that alignment."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Blindness/Deafness"
    , spLevel = 2
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V"
    , spDuration = "1 Minute"
    , spDescription = "You can blind or deafen a foe. Choose one creature that you can see within range to make a Constitution saving throw. If it fails, the target is either \\textbf{blinded} or \\textbf{deafened} (your choice) for the duration. At the end of each of its turns, the target can make a Constitution saving throw. On a success, the spell ends."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd."
    }
  , Spell
    { spName = "Blur"
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 Minute"
    , spDescription = "Your body becomes blurred, shifting and wavering to all who can see you. For the duration, any creature has disadvantage on attack rolls against you. An attacker is immune to this effect if it doesn't rely on sight, as with \\textbf{blindsight}, or can see through illusions, as with \\textbf{truesight}."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Cloud of Daggers"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (ruby dust worth 50gp, which the spell consumes)"
    , spDuration = "Until dispelled"
    , spDescription = "A flame, equivalent in brightness to a torch, springs forth from an object that you touch. The effect looks like a regular flame, but it creates no heat and doesn't use oxygen. A \\textbf{continual flame} can be covered or hidden but not smothered or quenched."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Crown of Madness"
    , spLevel = 2
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "One humanoid of your choice that you can see within range must succeed on a Wisdom saving throw or become charmed by you for the duration. While the target is charmed in this way, a twisted crown of jagged iron appears on its head, and madness glows in its eyes.\\\\The charmed target must use its action before moving on each of its turns to make a melee attack against a creature other than itself that you mentally choose. The target can act normally on its turn if you choose no creature or if none are within reach.\\\\On your subsequent turns, you must use your action to maintain control over the target, or the spell ends. Also, the target can make a Wisdom saving throw at the end of each of its turns. An a success, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Darkness"
    , spLevel = 2
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,M (bat fur and a drop of pitch or piece of coal)"
    , spDuration = "10 minutes"
    , spDescription = "Magical darkness spreads from a point you choose within range to fill a 15-foot-radius sphere for the duration. The darkness spreads around corners. A creature with \textbf{darkvision} can't see through this darkness, and nonmagical light can't illuminate it.\\\\If the point you choose is on an object you are holding or one that isn't being worn or carried, the darkness emanates from the object and moves with it. Completely covering the source of the darkness with an opaque object, such as a bowl or a helm, blocks the darkness.\\\\If any of this spell's area overlaps with an area of light created by a spell of 2nd level or lower, the spell that created the light is dispelled."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Darkvision"
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (either a pinch of dried carrot or an agate)"
    , spDuration = "8 hours"
    , spDescription = "You touch a willing creature to grant it the ability to see in the dark. For the duration, that creature has \\textbf{darkvision} out to a range of 60 feet."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Detect Thoughts"
    , spLevel = 2
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a copper piece)"
    , spDuration = "1 minute"
    , spDescription = "For the duration, you can read the thoughts of certain creatures. When you cast the spell and as your action on each turn until the spell ends, you can focus your mind on any one creature that you can see within 30 feet of you. If the creature you choose has an Intelligence of 3 or lower or doesn't speak any language, the creature is unaffected.\\\\You initially learn the surface thoughts of the creature--what is most on its mind in that moment. As an action, you can either shift your attention to another creature's thoughts or attempt to probe deeper into the same creature's mind. If you probe deeper, the target must make a Wisdom saving throw. If it fails, you gain insight into its reasoning (if any), its emotional state, and something that looms large in its mind (such as something it worries over, loves, or hates). If it succeeds, the spell ends. Either way, the target knows that you are probing into its mind, and unless you shift your attention to another creature's thoughts, the creature can use its action on its turn to make an Intelligence check contested by your Intelligence check; if it succeeds, the spell ends.\\\\Questions verbally directed at the target creature naturally shape the course of its thoughts, so this spell is particularly effective as part of an interrogation.\\\\You can also use this spell to detect the presence of thinking creatures you can't see. When you cast the spell or as your action during the duration, you can search for thoughts within 30 feet of you. The spell can penetrate barriers, but 2 feet of rock, 2 inches of any metal other than lead, or a thin sheet of lead blocks you. You can't detect a creature with an Intelligence of 3 or lower or one that doesn't speak any language.\\\\Once you detect the presence of a creature in this way, you can read its thoughts for the rest of the duration as described above, even if you can't see it, but it must still be within range."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Dragon's Breath"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a pinch of dust)"
    , spDuration = "1 minute"
    , spDescription = "Choose an unoccupied 5-foot cube of air that you can see within range. An elemental force that resembles a dust devil appears in the cube and lasts for the spell’s duration.\\\\Any creature that ends its turn within 5 feet of the dust devil must make a Strength saving throw. On a failed save, the creature takes 1d8 bludgeoning damage and is pushed 10 feet away. On a successful save, the creature takes half as much damage and isn’t pushed.\\\\As a bonus action, you can move the dust devil up to 30 feet in any direction. If the dust devil moves over sand, dust, loose dirt, or small gravel, it sucks up the material and forms a 10-foot-radius cloud of debris around itself that lasts until the start of your next turn. The cloud heavily obscures its area."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
  , Spell
    { spName = "Earthbind"
    , spLevel = 2
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
    { spName = "Enlarge/Reduce"
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a pinch of powdered iron)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You cause a creature or an object you can see within range to grow larger or smaller for the duration. Choose either a creature or an object that is neither worn nor carried. If the target is unwilling, it can make a Constitution saving throw. On a success, the spell has no effect.\\\\If the target is a creature, everything it is wearing and carrying changes size with it. Any item dropped by an affected creature returns to normal size at once.\\\\\\textbf{Enlarge.} The target's size doubles in all dimensions, and its weight is multiplied by eight. This growth increases its size by one category-- from Medium to Large, for example. If there isn't enough room for the target to double its size, the creature or object attains the maximum possible size in the space available. Until the spell ends, the target also has advantage on Strength checks and Strength saving throws. The target's weapons also grow to match its new size. While these weapons are enlarged, the target's attacks with them deal 1d4 extra damage.\\\\\\textbf{Reduce.} The target's size is halved in all dimensions, and its weight is reduced to one-eighth of normal. This reduction decreases its size by one category--from Medium to Small, for example. Until the spell ends, the target also has disadvantage on Strength checks and Strength saving throws. The target's weapons also shrink to match its new size. While these weapons are reduced, the target's attacks with them deal 1d4 less damage (this can't reduce the damage below 1)."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Flaming Sphere"
    , spLevel = 2
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of tallow, a pinch of brimstone, and a dusting of powdered iron)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A 5-foot-diameter sphere of fire appears in an unoccupied space of your choice within range and lasts for the duration. Any creature that ends its turn within 5 feet of the sphere must make a Dexterity saving throw. The creature takes 2d6 fire damage on a failed save, or half as much damage on a successful one.\\\\As a bonus action, you can move the sphere up to 30 feet. If you ram the sphere into a creature, that creature must make the saving throw against the sphere's damage, and the sphere stops moving this turn.\\\\When you move the sphere, you can direct it over barriers up to 5 feet tall and jump it across pits up to 10 feet wide. The sphere ignites flammable objects not being worn or carried, and it sheds bright light in a 20-foot radius and dim light for an additional 20 feet."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d6 for each slot level above 2nd."
    }
  , Spell
    { spName = "Flock of Familiars (Lost Laboratory of Kwalish)"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a pinch of salt and one copper piece placed on each of the corpse's eyes, which must remain there for the duration)"
    , spDuration = "10 days"
    , spDescription = "You touch a corpse or other remains. For the duration, the target is protected from decay and can't become undead.\\\\The spell also effectively extends the time limit on raising the target from the dead, since days spent under the influence of this spell don't count against the time limit of spells such as \\textbf{raise dead}."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Gift of Gab (Acquisitions Incorporated)"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a legume seed)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A line of strong wind 60 feet long and 10 feet wide blasts from you in a direction you choose for the spell's duration. Each creature that starts its turn in the line must succeed on a Strength saving throw or be pushed 15 feet away from you in a direction following the line.\\\\Any creature in the line must spend 2 feet of movement for every 1 foot it moves when moving closer to you.\\\\The gust disperses gas or vapor, and it extinguishes candles, torches, and similar unprotected flames in the area. It causes protected flames, such as those of lanterns, to dance wildly and has a 50 percent chance to extinguish them.\\\\As a bonus action on each of your turns before the spell ends, you can change the direction in which the line blasts from you."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Hold Person"
    , spLevel = 2
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a small. straight piece of iron)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose a humanoid that you can see within range. The target must succeed on a Wisdom saving throw or be \\textbf{paralyzed} for the duration. At the end of each of its turns, the target can make another Wisdom saving throw. On a success, the spell ends on the target."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional humanoid for each slot level above 2nd. The humanoids must be within 30 feet of each other when you target them."
    }
  , Spell
    { spName = "Invisibility"
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (an eyelash encased in gum arabic)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "A creature you touch becomes \\textbf{invisible} until the spell ends. Anything the target is wearing or carrying is \\textbf{invisible} as long as it is on the target's person. The spell ends for a target that attacks or casts a spell."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd."
    }
  , Spell
    { spName = "Jim's Glowing Coin (Acquisitions Incorporated)"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spDescription = "Choose an object that you can see within range. The object can be a door, a box, a chest, a set of manacles, a padlock, or another object that contains a mundane or magical means that prevents access.\\\\A target that is held shut by a mundane lock or that is stuck or barred becomes unlocked, unstuck, or unbarred. If the object has multiple locks, only one of them is unlocked.\\\\If you choose a target that is held shut with \textit{arcane lock}, that spell is suppressed for 10 minutes, during which time the target can be opened and shut normally.\\\\When you cast the spell, a loud knock, audible from as far away as 300 feet, emanates from the target object."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Levitate"
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (either a small leather loop or a piece of golden wire bent into a cup shape with a long shank on one end)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "One creature or loose object of your choice that you can see within range rises vertically, up to 20 feet, and remains suspended there for the duration. The spell can levitate a target that weighs up to 500 pounds. An unwilling creature that succeeds on a Constitution saving throw is unaffected.\\\\The target can move only by pushing or pulling against a fixed object or surface within reach (such as a wall or a ceiling), which allows it to move as if it were climbing. You can change the target's altitude by up to 20 feet in either direction on your turn. If you are the target, you can move up or down as part of your move. Otherwise, you can use your action to move the target, which must remain within the spell's range.\\\\When the spell ends, the target floats gently to the ground if it is still aloft."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Locate Object"
    , spLevel = 2
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a forked twig)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "Describe or name an object that is familiar to you. You sense the direction to the object's location, as long as that object is within 1,000 feet of you. If the object is in motion, you know the direction of its movement.\\\\The spell can locate a specific object known to you, as long as you have seen it up close--within 30 feet--at least once. Alternatively, the spell can locate the nearest object of a particular kind, such as a certain kind of apparel, jewelry, furniture, tool, or weapon.\\\\This spell can't locate an object if any thickness of lead, even a thin sheet, blocks a direct path between you and the object."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Magic Mouth"
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a small bit of honeycomb and jade dust worth at least 10 gp, which the spell consumes)"
    , spDuration = "Until dispelled"
    , spDescription = "You implant a message within an object in range, a message that is uttered when a trigger condition is met. Choose an object that you can see and that isn't being worn or carried by another creature. Then speak the message, which must be 25 words or less, though it can be delivered over as long as 10 minutes. Finally, determine the circumstance that will trigger the spell to deliver your message.\\\\When that circumstance occurs, a magical mouth appears on the object and recites the message in your voice and at the same volume you spoke. If the object you chose has a mouth or something that looks like a mouth (for example, the mouth of a statue), the magical mouth appears there so that the words appear to come from the object's mouth. When you cast this spell, you can have the spell end after it delivers its message, or it can remain and repeat its message whenever the trigger occurs.\\\\The triggering circumstance can be as general or as detailed as you like, though it must be based on visual or audible conditions that occur within 30 feet of the object. For example, you could instruct the mouth to speak when any creature moves within 30 feet of the object or when a silver bell rings within 30 feet of it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Magic Weapon"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a miniature hand sculpted from clay)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You choose a 5-foot-square unoccupied space on the ground that you can see within range. A Medium hand made from compacted soil rises there and reaches for one creature you can see within 5 feet of it. The target must make a Strength saving throw. On a failed save, the target takes 2d6 bludgeoning damage and is \textbf{restrained} for the spell’s duration.\\\\As an action, you can cause the hand to crush the \textbf{restrained} target, who must make a Strength saving throw. It takes 2d6 bludgeoning damage on a failed save, or half as much damage on a successful one.\\\\To break out, the \\textbf{restrained} target can use its action to make a Strength check against your spell save DC. On a success, the target escapes and is no longer \\textbf{restrained} by the hand.\\\\As an action, you can cause the hand to reach for a different creature or to move to a different unoccupied space within range. The hand releases a \\textbf{restrained} target if you do either."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Melf's Acid Arrow"
    , spLevel = 2
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
    , spLevel = 2
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
    , spLevel = 2
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
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spDescription = "Three illusory duplicates of yourself appear in your space. Until the spell ends, the duplicates move with you and mimic your actions, shifting position so it's impossible to track which image is real. You can use your action to dismiss the illusory duplicates.\\\\Each time a creature targets you with an attack during the spell's duration, roll a d20 to determine whether the attack instead targets one of your duplicates.\\\\If you have three duplicates, you must roll a 6 or higher to change the attack's target to a duplicate. With two duplicates, you must roll an 8 or higher. With one duplicate, you must roll an 11 or higher.\\\\A duplicate's AC equals 10 + your Dexterity modifier. If an attack hits a duplicate, the duplicate is destroyed. A duplicate can be destroyed only by an attack that hits it. It ignores all other damage and effects. The spell ends when all three duplicates are destroyed.\\\\A creature is unaffected by this spell if it can't see, if it relies on senses other than sight, such as \\textbf{blindsight}, or if it can perceive illusions as false, as with \\textbf{truesight}."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Misty Step"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a small square of silk)"
    , spDuration = "24 hours"
    , spDescription = "You place an illusion on a creature or an object you touch so that divination spells reveal false information about it. The target can be a willing creature or an object that isn't being carried or worn by another creature.\\\\When you cast the spell, choose one or both of the following effects. The effect lasts for the duration. If you cast this spell on the same creature or object every day for 30 days, placing the same effect on it each time, the illusion lasts until it is dispelled.\\\\\\textbf{False Aura.} You change the way the target appears to spells and magical effects, such as \\textit{detect magic}, that detect magical auras. You can make a nonmagical object appear magical, a magical object appear nonmagical, or change the object's magical aura so that it appears to belong to a specific school of magic that you choose. When you use this effect on an object, you can make the false magic apparent to any creature that handles the item.\\\\\\textbf{Mask.} You change the way the target appears to spells and magical effects that detect creature types, such as a paladin's Divine Sense or the trigger of a \\textit{symbol} spell. You choose a creature type and other spells and magical effects treat the target as if it were a creature of that type or of that alignment."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Phantasmal Force"
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of fleece)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You craft an illusion that takes root in the mind of a creature that you can see within range. The target must make an Intelligence saving throw. On a failed save, you create a phantasmal object, creature, or other visible phenomenon of your choice that is no larger than a 10-foot cube and that is perceivable only to the target for the duration. The spell has no effect on undead or constructs.\\\\The phantasm includes sound, temperature and other stimuli, also evident only to the creature.\\\\The target can use its action to examine the phantasm as if it were real. The target rationalizes any illogical outcomes from interacting with the phantasm. For example, a target attempting to walk across a phantasmal bridge that spans a chasm falls once it steps on to the bridge. If the target survives the fall, it still believes that the bridge exists and comes up with some other explanation for its fall - it was pushed, it slipped, or a strong wind might have knocked it off.\\\\An affected target is so convinced of the phantasm's reality that it can even take damage from the illusion. A phantasm created to appear as fire, a pool of acid, or lava can burn the target. Each round on your turn, the phantasm can deal 1d6 psychic damage to the target if it is in the phantasm's area or within 5 feet of the phantasm, provided that the illusion is of a creature or hazard that could logically deal damage, such as by attacking. The target perceives the damage as a type appropriate to the illusion."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Pyrotechnics"
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "Choose an area of nonmagical flame that you can see and that fits within a 5-foot cube within range. You can extinguish the fire in that area, and you create either fireworks or smoke when you do so.\\\\\\textbf{Fireworks.} The target explodes with a dazzling display of colors. Each creature within 10 feet of the target must succeed on a Constitution saving throw or become \\textbf{blinded} until the end of your next turn.\\\\\\textbf{Smoke.} Thick black smoke spreads out from the target in a 20-foot radius, moving around corners. The area of the smoke is heavily obscured. The smoke persists for 1 minute or until a strong wind disperses it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Ray of Enfeeblement"
    , spLevel = 2
    , spType = Necromancy
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A black beam of enervating energy springs from your finger toward a creature within range. Make a ranged spell attack against the target. On a hit, the target deals only half damage with weapon attacks that use Strength until the spell ends.\\\\At the end of each of the target's turns, it can make a Constitution saving throw against the spell. On a success, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Rope Trick"
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (powdered corn extract and a twisted loop of parchment)"
    , spDuration = "1 hour"
    , spDescription = "You touch a length of rope that is up to 60 feet long. One end of the rope then rises into the air until the whole rope hangs perpendicular to the ground. At the upper end of the rope, an invisible entrance opens to an extradimensional space that lasts until the spell ends.\\\\The extradimensional space can be reached by climbing to the top of the rope. The space can hold as many as eight Medium or smaller creatures. The rope can be pulled into the space, making the rope disappear from view outside the space.\\\\Attacks and spells can't cross through the entrance into or out of the extradimensional space, but those inside can see out of it as if through a 3-foot-by-5- foot window centered on the rope.\\\\Anything inside the extradimensional space drops out when the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Scorching Ray"
    , spLevel = 2
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You create three rays of fire and hurl them at targets within range. You can hurl them at one target or several.\\\\Make a ranged spell attack for each ray. On a hit, the target takes 2d6 fire damage."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you create one additional ray for each slot level above 2nd."
    }
  , Spell
    { spName = "See Invisibility"
    , spLevel = 2
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a pinch of talc and a small sprinkling of powdered silver)"
    , spDuration = "1 hour"
    , spDescription = "For the duration, you see \\textbf{invisible} creatures and objects as if they were visible, and you can see into the Ethereal Plane. Ethereal creatures and objects appear ghostly and translucent."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Shadow Blade (Xanathar's)"
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You weave together threads of shadow to create a sword of solidified gloom in your hand. The magic sword lasts until the spell ends. It counts as a simple melee weapon with which you are proficient. It deals 2d8 psychic damage on a hit and has the finesse, light and thrown properties (range 20/60). In addition, when you use the sword to attack a target that is in dim light or darkness, you make the attack roll with advantage.\\\\If you drop the weapon or throw it, it dissapates at the end of the turn. Thereafter, while the spell persists, you can use your bonus action to cause the sword to reappear in your hand."
    , spHigher = Just "When you cast this spell using a 3rd or 4th level slot, the damage increases to 3d8. When you cast this spell using a 5th or 6th level slot, the damage increases to 4d8. When you cast this spell using a spell slot of 7th level or higher, the damage increases to 5d8."
    }
  , Spell
    { spName = "Shatter"
    , spLevel = 2
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a chip of mica)"
    , spDuration = "Instantaneous"
    , spDescription = "A sudden loud ringing noise, painfully intense, erupts from a point of your choice within range. Each creature in a 10-foot-radius sphere centered on that point must make a Constitution saving throw. A creature takes 3d8 thunder damage on a failed save, or half as much damage on a successful one. A creature made of inorganic material such as stone, crystal, or metal has disadvantage on this saving throw.\\\\A nonmagical object that isn't being worn or carried also takes the damage if it's in the spell's area."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
  , Spell
    { spName = "Skywrite"
    , spLevel = 2
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
    , spLevel = 2
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
    , spLevel = 2
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
    , spLevel = 2
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 8 hours"
    , spDescription = "You suggest a course of activity (limited to a sentence or two) and magically influence a creature you can see within range that can hear and understand you. Creatures that can't be \\textbf{charmed} are immune to this effect. The suggestion must be worded in such a manner as to make the course of action sound reasonable. Asking the creature to stab itself, throw itself onto a spear, immolate itself, or do some other obviously harmful act ends the spell.\\\\The target must make a Wisdom saving throw. On a failed save, it pursues the course of action you described to the best of its ability. The suggested course of action can continue for the entire duration. If the suggested activity can be completed in a shorter time, the spell ends when the subject finishes what it was asked to do.\\\\You can also specify conditions that will trigger a special activity during the duration. For example, you might suggest that a knight give her warhorse to the first beggar she meets. If the condition isn't met before the spell expires, the activity isn't performed.\\\\If you or any of your companions damage the target, the spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Warding Wind"
    , spLevel = 2
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "A strong wind (20 miles per hour) blows around you in a 10-foot radius and moves with you, remaining centered on you. The wind lasts for the spell’s duration.\\\\The wind has the following effects:\\begin{itemize}\\item It deafens you and other creatures in its area.\\item It extinguishes unprotected flames in its area that are torch-sized or smaller.\\item It hedges out vapor, gas, and fog that can be dispersed by strong wind.\\item The area is difficult terrain for creatures other than you.\\item The attack rolls of ranged weapon attacks have disadvantage if the attacks pass in or out of the wind.\\end{itemize}."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Web"
    , spLevel = 2
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of spiderweb)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You conjure a mass of thick, sticky webbing at a point of your choice within range. The webs fill a 20-foot cube from that point for the duration. The webs are difficult terrain and lightly obscure their area.\\\\If the webs aren't anchored between two solid masses (such as walls or trees) or layered across a floor, wall, or ceiling, the conjured web collapses on itself, and the spell ends at the start of your next turn. Webs layered over a flat surface have a depth of 5 feet.\\\\Each creature that starts its turn in the webs or that enters them during its turn must make a Dexterity saving throw. On a failed save, the creature is \textbf{restrained} as long as it remains in the webs or until it breaks free.\\\\A creature \\textbf{restrained} by the webs can use its action to make a Strength check against your spell save DC. If it succeeds, it is no longer restrained.\\\\The webs are flammable. Any 5-foot cube of webs exposed to fire burns away in 1 round, dealing 2d4 fire damage to any creature that starts its turn in the fire."
    , spHigher = Nothing
    }
  ]