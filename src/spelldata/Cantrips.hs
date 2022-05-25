{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module Cantrips where

import SpellTypes

cantrips :: [Spell]
cantrips =
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
    , spDescription = "You hurl a bubble of acid. Choose one creature within range, or choose two creatures within range that are within 5 feet of each other. A target must succeed on a Dexterity saving throw or take 1d6 acid damage.\n\nThis spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
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
    , spDescription = "You create a bonfire on ground that you can see within range. Until the spell ends, the magic bonfire fills a 5-foot cube. Any creature in the bonfire’s space when you cast the spell must succeed on a Dexterity saving throw or take 1d8 fire damage. A creature must also make the saving throw when it moves into the bonfire’s space for the first time on a turn or ends its turn there.\n\nThe bonfire ignites flammable objects in its area that aren’t being worn or carried.\n\nThe spell’s damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
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
    , spDescription = "You create up to four torch-sized lights within range, making them appear as torches, lanterns or glowing orbs that hover in the air for the duration. You can also combine the four lights into one glowing vaguely humanoid form of medium size. Whichever form you choose, each light sheds dim light in a 10-foot radius.\n\nAs a bonus action on your turn, you can move the lights up to 60 feet to a new spot within range. A light must be within 20 feet of another light created by this spell, and a light winks out if it exceeds the spell's range."
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
    { spName = "Eldritch Blast"
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "A beam of crackling energy streaks toward a creature within range. Make a ranged spell attack against the target. On a hit, the target takes 1d10 force damage.\n\nThe spell creates more than one beam when you reach higher levels: two beams at 5th level, three beams at 11th level, and four beams at 17th level. You can direct the beams at the same target or at different ones. Make a separate attack roll for each beam."
    , spHigher = Nothing
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
    , spDescription = "You cause numbing frost to form on one creature that you can see within range. The target must make a Constitution saving throw. On a failed save, the target takes 1d6 cold damage, and it has disadvantage on the next weapon attack roll it makes before the end of its next turn.\n\nThe spell’s damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
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
    { spName = "Guidance"
    , spLevel = 0
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
    , spDescription = "You cause a cloud of mites, fleas, and other parasites to appear momentarily on one creature you can see within range. The target must succeed on a Constitution saving throw, or it takes 1d6 poison damage and moves 5 feet in a random direction if it can move and its speed is at least 5 feet. Roll a d4 for the direction: 1, north; 2: south; 3, east; or 4, west. This movement doesn't provoke opportunity attacks, and if the direction rolled is blocked, the target doesn't move.\n\nThe spell's damage increases by 1d6 when you reach 5th level (2d6), 11th level (3d6) and 17th level (4d6)."
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
    , spDescription = "You touch one object that is not larger than 10 feet in any dimension. Until the spell ends, the object sheds bright light in a 20-foot radius and dim light for an additional 20 feet. The light can be colored as you like. Completely covering the object with something opaque blocks the light. The spell ends if you cast it again or dismiss it as an action.\n\nIf you target an object worn or held by a hostile creature, that creature must succeed on a Dexterity saving throw to avoid the spell."
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
    , spDescription = "A spectral, floating hand appears at a point you choose within range. The hand lasts for the duration or until you dismiss it as an action. The hand vanishes if it is ever more than 30 feet away from you or if you cast this spell again.\n\nYou can use your action to control the hand. You can use the hand to manipulate an object, open an unlocked door or container, stow or retrieve an item from an open contrainer, or pour the contents out of a vial. You can move the hand up to 30 feet each time you use it.\n\nYou can't attack, activate magic items, or carry more than 10 pounds."
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
    , spDescription = "This spell repairs a single break or tear in an object you touch, such as a broken chain link, two halves of a broken key, a torn cloak, or a leaking wineskin. As long as the break or tear is no larger than 1 foot in any dimension, you mend it, leaving no trace of former damage.\n\nThis spell can physically repair a magic item or construct, but the spell can't restore magic to such an object."
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
    , spDescription = "You point your finger toward a creature within range and whisper a message. The target (and only the target) hears the message and can reply in a whisper that only you can hear.\n\nYou can cast this spell through solid objects if you are familiar with the target and know it is beyond the barrier. Magical silence, 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood blocks the spell. The spell doesn't have to follow a straight line and can freely travel around corners or through openings."
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
    , spDescription = "You drive a disorienting spike of psychic energy into the mind of one creature you can see within range. The target must make an Intelligence saving throw. Unless the saving throw is successful, the target takes 1d6 psychic damage, and the first time it makes a saving throw before the end of your next turn, it must roll a d4 and subtract the number rolled from the save.\n\nThis spell’s damage increases by 1d6 when you reach certain levels: 5th level (2d6), 11th level (3d6), and 17th level (4d6)."
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
    , spDescription = "You create a sound or image of an object within range that lasts for the duration. The illusion also ends if you dismiss it as an action or cast this spell again.\n\nIf you create a sound, its volume can range from a whisper to a scream. It can be your voice, someone else's voice, a lion's roar, a beating of drums, or any other sound you choose. The sound continues unabated throughout the duration, or you can make discrete sounds at different times before the spell ends.\n\nIf you create an image of an object - such as a chair, muddy footprints or a small chest - it must be no larger than a 5-foot cube. The image can't create sound, light, smell, or any other sensory effect. Physical interaction with the image reveals it to be an illusion, because things can pass through it.\n\nIf a creature uses its action to examine the sound or image, the creature can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a create discerns the illusion for what it is, the illusion becomes faint to the creature."
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
    , spDescription = "You extend your hand toward a creature you can see within range and prject a puff of noxious gas from your palm. The creature must succeed on a Constitution saving throw or take 1d12 poison damage.\n\nThis spell's damage increases by 1d12 when you reach 5th level (2d12), 11th level (3d12), and 17th level (4d12)."
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
    { spName = "Produce Flame"
    , spLevel = 0
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "10 minutes"
    , spHigher = Nothing
    , spDescription = "A flickering flame appears in your hand. The flame remains there for the duration and harms neither you nor your equipment. The flame sheds bright light in a 10-foot radius and dim light for an additional 10 feet. The spell ends if you dismiss it as an action or if you cast it again.\n\n"
      <> "You can also attack with the flame, although doing so ends the spell. When you cast this spell, or as an action on a later turn, you can hurl the flame at a creature within 30 feet of you. Make a ranged spell attack. On a hit, the target takes 1d8 fire damage.\n\n"
      <> "This spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
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
    , spDescription = "A frigid beam of blue-white light streaks toward a creature within range. Make a ranged spell attack against the target. On a hit, it takes 1d8 cold damage, and its speed is reduced by 10 feet until the start of your next turn.\n\nThe spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8) and 17th level (4d8)."
    }
  , Spell 
    { spName = "Resistance"
    , spLevel = 0
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
    , spLevel = 0
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spHigher = Nothing
    , spDescription = "Flame-like radiance descends on a creature that you can see within range. The target must succeed on a Dexterity saving throw or take 1d8 radiant damage. The target gains no benefit from cover for this saving throw.\n\nThe spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
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
    , spDescription = "Lightning springs from your hand to deliver a shock to a creature you try to touch. Make a melee spell attack against the target. You have advantage on the attack roll if the target is wearing armour made of metal. On a hit, the target takes 1d8 lightning damage, and can't take reactions until the start of its next turn.\n\nThis spell's damage increases by 1d8 when you reach 5th level (2d8), 11th level (3d8), and 17th level (4d8)."
    }
  , Spell 
    { spName = "Spare the Dying"
    , spLevel = 0
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
    , spDescription = "You point at one creature you can see within range, and the sound of a dolorous bell fills the air around it for a moment. The target must succeed on a Wisdom saving throw or take 1d8 necrotic damage. If the target is missing any of its hit points, it instead takes 1d12 necrotic damage.\n\nThe spell's damage increases by one die when you reach 5th level (2d8 or 2d12), 11th level (3d8 or 3d12) and 17th level (4d8 or 4d12)."
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
  , Spell 
    { spName = "Word of Radiance"
    , spLevel = 0
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