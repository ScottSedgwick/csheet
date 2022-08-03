{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module FirstLevel where

import SpellTypes

firstLevel :: [Spell]
firstLevel =
  [ Spell
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
    , spDescription = "You set an alarm against unwanted intrusion. Choose a door, window, or an area within range that is no larger than a 20-foot cube. Until the spell ends, an alarm alerts you whenever a Tiny or larger creature touches or enters the warded area. When you cast the spell, you can designate creatures that won't set off the alarm. You also choose whether the alarm is mental or audible.\n\nA mental alarm alerts you with a ping in your mind if you are within 1 mile of the warded area. This ping awakens you if you are sleeping.\n\nAn audible alarm produces the sound of a hand bell for 10 seconds within 60 feet."
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
    , spDescription = "As you hold your hands with thumbs touching and fingers spread, a thin sheet of flames shoots forth from your outstretched fingertips. Each creature in a 15-foot cone must make a Dexterity saving throw. A creature takes 3d6 fire damage on a failed save, or half as much damage on a successful one.\n\nThe fire ignites any flammable objects in the area that aren't being worn or carried"
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
    { spName = "Chaos Bolt"
    , spLevel = 1
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You hurl an undulating, warbling mass of chaotic energy at one creature in range. Make a ranged spell attack against the target. On a hit, the target takes 2d8 + 1d6 damage. Choose one of the d8s. The number rolled on that die determines the attack’s damage type, as shown below.\n\n" <>
      "| d8  | Damage Type |\n" <>
      "| --- | ----------- |\n" <>
      "|  1  | Acid        |\n" <>
      "|  2  | Cold        |\n" <>
      "|  3  | Fire        |\n" <>
      "|  4  | Force       |\n" <>
      "|  5  | Lightning   |\n" <>
      "|  6  | Poison      |\n" <>
      "|  7  | Psychic     |\n" <>
      "|  8  | Thunder     |\n\n" <>
      "If you roll the same number on both d8s, the chaotic energy leaps from the target to a different creature of your choice within 30 feet of it. Make a new attack roll against the new target, and make a new damage roll, which could cause the chaotic energy to leap again.\n\n" <>
      "A creature can be targeted only once by each casting of this spell."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, each target takes 1d6 extra damage of the type rolled for each slot level above 1st."
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
    , spDescription = "A dazzling array of flashing, coloured light springs from your hand. Roll 6d10; the total is how many hit points of creatures this spell can affect. Creatures in a 15-foot cone originating from you are affected in ascending order of their current hit points (ignoring unconscious creatures and creatures that can't see).\n\nStarting with the creature that has the lowest current hit points, each creature affected by this spell is blinded until the spell ends. Subtract each creature's hit points from the total before moving to the creature with the next lowest hit points. A creature's hit points must be equal to or less that the remaining total for that creature to be affected."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, roll an additional 2d10 for each slot level above 1st."
    }
  , Spell
    { spName = "Command"
    , spLevel = 1
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
    , spLevel = 1
    , spType = Divination
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a pinch of soot and salt)"
    , spDuration = "1 hour"
    , spDescription = "For the duration, you understand the literal meaning of any spoken language that you hear. You also understand any written language that you see, but you must be touching the surface on which the words are written. It takes about 1 minute to read one page of text.\n\nThis spell doesn't decode secret messages in a text or a glyph, such as an arcane sigil, that isn't part of a written langauage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Create/Destroy Water"
    , spLevel = 1
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a drop of water if creating water or a few grains of sand if destroying it)"
    , spDuration = "Instantaneous"
    , spDescription = "**Create Water.** You create up to 10 gallons of clean water within range in an open container. Alternatively, the water falls as rain in a 30-foot cube within range, extinguishing exposed flames in the area.\n\n" 
      <> "**Destroy Water.** You destroy up to 10 gallons of water in an open container within range. Alternatively, you destroy fog in a 30-foot cube within range."
    , spHigher = Just "When you cast this spell using a spell slot of 2nd level or higher, you create or destroy 10 additional gallons of water, or the size of the cube increases by 5 feet, for each slot level above 1st."
    }
  , Spell
    { spName = "Cure Wounds"
    , spLevel = 1
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
    , spLevel = 1
    , spType = Divination
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "For the duration, you sense the presence of magic within 30 feet of you. If you sense magic in this way, you can use your action to see a faint aura around any visible creature or object in the area that bears magic, and you learn its school of magic, if any.\n\nThe spell can penetrate most barriers, but it is blocked by 1 foot of stone, 1 inch of common metal, a thin sheet of lead, or 3 feet of wood or dirt."
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
    , spDescription = "You make yourself - including your clothing, armour, weapons and other belongings on your person - look different until the spell ends or you use your action to dismiss it. You can seem 1 foot shorter or taller and can appear thin, fat or in between. You can't change your body type, so you must adopt a form that has the same basic arrangement of limbs. Otherwise, the extent of the illusion is up to you.\n\nThe changes wrought by this spell fail to hold up to physical inspection. For example, if you use this spell to add a hat to your outfit, objects pass through the hat, and anyone who touches it would feel nothing or would feel your hear and hair. If you use this spell to appear thinner than you are, the hand of someone who reaches out to touch you would bump into you while it was seemingly still in midair.\n\nTo discern that you are disguised, a creature can use its action to inspect your appearance and must succeed on an Intelligence (Investigation) check against your spell save DC."
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
    { spName = "Faerie Fire"
    , spLevel = 1
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Each object in a 20-foot cube within range is outlined in blue, green, or violet light (your choice). Any creature in the area when the spell is cast is also outlined in light if it fails a Dexterity saving throw. For the duration, objects and affected creatures shed dim light in a 10-foot radius.\n\n"
      <> "Any attack roll against an affected creature or object has advantage if the attacker can see it, and the affected creature or object can't benefit from being invisible."
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
    , spDescription = "You gain the service of a familiar, a spirit that takes an animal form you choose: bat, cat crab, frog (toad), hawk, lizard, octopus, owl, poisonous snake, fish (quipper), rat, raven, sea horse, spider or weasel. Appearing in an unoccupied space within range, the familiar has the statistics of the chosen form, though it is a celestial, fey or fiend (your choice) instead of a beast.\n\nYour familiar acts independently of you, but it always obeys your commands. In combat, it rools its own initiative and acts on its own turn. A familiar can't attack, but it can take other actions as normal.\n\nWhen the familiar drops to 0 hit points, it disappears, leaving behind no physical form. It reappears after you cast this spell again.\n\nWhile your familiar is within 100 feet of you, you can communicate with it telepathically. Additionally, as an action, you can see through your familiar's eyes and hear what it hears until the start of your next turn, gaining the benefits of any special senses that the familiar has. During this time, you are deaf and blind with regard to your own senses.\n\nAs an action, you can temprarily dismiss your familiar. It disappears into a pocket dimension where it awaits your summons. Alternatively, you can dismiss it forever. As an action while it is temporarily dismissed, you can cause it to appear in any unoccupied space within 30 feet of you.\n\nYou can't have more than one familiar at a time. If you cast this spell while you already have a familiar, you instead cause it to adopt a new form. Choose one of the forms from the above list. Your familiar transforms into the chosen creature.\n\nFinally, when you cast a spell with a range of touch, your familiar can deliver the spell as if it had cast the spell. Your familiar must be within 100 feet of you, and it must use its reaction to deliver the spell when you cast it. If the spell requires an attack roll, you use your attack modifier for the roll."
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
    , spDescription = "Slick grease covers the ground in a 10-foot square centered on a point within range and turns it into difficult terrain for the duration.\n\nWhen the grease appears, each creature standing in its area must succeed on a Dexterity saving throw or fall prone. A creature that enters the area or ends its turn there must also succeed on a Dexterity saving throw or fall prone."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Guiding Bolt"
    , spLevel = 1
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
    { spName = "Hex"
    , spLevel = 1
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "Touch"
    , spComponents = "V,S,M (the petrified eye of a newt)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You place a curse on a creature that you can see within range. Until the spell ends, you deal an extra 1d6 necrotic damage to the target whenever you hit it with an attack. Also, choose one ability when you cast the spell. The target has disadvantage on ability checks made with the chosen ability.\n\nIf the target drops to 0 hit points before this spell ends, you can use a bonus action on a subsequent turn of yours to curse a new creature.\n\nA Remove Curse cast on the target ends this spell early."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd or 4th level, you can maintain your concentration on the spell for up to 8 hours. When you use a spell slot of 5th level or higher, you can maintain your concentration on the spell for up to 24 hours."
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
    , spDescription = "You choose one object that you must touch throughout the casting of the spell. If it is a magic item or some other magic-imbued object, you learn its properties and how to use them, whether it requires attunement to use, and how many charges it has, if any. You learn whether any spells are affecting the item and what they are. If the item was created by a spell, you learn which spell created it.\n\nIf you instead touch a creature throughout the casting, you learn what spells, if any, are currently affecting it."
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
    , spDescription = "You write on parchment, paper or some other suitable writing material and imbue it with a potent illusion that lasts for the duration.\n\nTo you and any creatures you designate when you cast the spell, the writing appears normal, written in your hand, and conveys whatever meaning you intended when you wrote the text. To all others, the writing appears as if it were in an unknown or magical script that is unintelligible. Alternatively, you can cause the writing to appear to be an entirely different message, written in a different hand and language, thought the language must be one that you know.\n\nShould the spell be dispelled, the original script and the illusion both disappear.\n\nA creature with truesight can read the hidden message."
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
    { spName = "Mage Armor"
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
    , spDescription = "Until the spell ends, one willing creature you touch is protected against certain types of creatures: abberations, celestials, elementals, fey, fiends and undead.\n\nThe protection grants several benefits. Creatures of those types have disadvantage on attack rools against the target. The target also can't be charmed, frightened, or possessed by them. If the target is already charmed, frightened, or possessed by such a creature, the target has advantage on any new saving throw against the relevant effect."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Purify Food and Drink"
    , spLevel = 1
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
    { spName = "Sanctuary"
    , spLevel = 1
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 bonus action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a small silver mirror)"
    , spDuration = "1 minute"
    , spDescription = "You ward a creature within range against attack. Until the spell ends, any creature who targets the warded creature with an attack or a harmful spell must first make a Wisdom saving throw. On a failed save, the creature must choose a new target or lose the attack or spell. This spell doesn't protect the warded creature from area effects, such as the explosion of a fireball.\n\n"
      <> "If the warded creature makes an attack, casts a spell that affects an enemy, or deals damage to another creature, this spell ends."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Searing Smite"
    , spLevel = 1
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
    { spName = "Shield of Faith"
    , spLevel = 1
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
    , spLevel = 1
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a bit of fleece)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "You create the image of an object, a creature, or some other visible phenomenon that is no larger than a 15-foot cube. The image appears at a spot within range and lasts for the duration. The image is purely visual; it isn't accompanied by sounds, smell, or other sensory effects.\n\nYou can use your action to cause the image to move to any spot within range. As the image changes location, you can alter its appearance so that its movements appear natural for the image. For example, if you create an image of a creature and move it, you can alter the image so that it appears to be walking.\n\nPhysical interaction with the image reveals it to be an illusion, because things can pass through it. A creature that uses its action to examine the image can determine that it is an illusion with a successful Intelligence (Investigation) check against your spell save DC. If a creature discerns the illusion for what it is, the creature can see through the image."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Silvery Barbs"
    , spLevel = 1
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 reaction, which you take when a creature you can see within 60 feet of yourself succeeds on an attack roll, an ability check, or a saving throw"
    , spRange = "60 feet"
    , spComponents = "V"
    , spDuration = "Instantaneous"
    , spDescription = "You magically distract the triggering creature and turn its momentary uncertainty into encouragement for another creature. The triggering creature must reroll the d20 and use the lower roll.\n\n"
      <> "You can then choose a different creature you can see within range (you can choose yourself). The chosen creature has advantage on the next attack roll, ability check, or saving throw it makes within 1 minute. A creature can be empowered by only one use of this spell at a time."
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
    , spDescription = "This spell sends creatures into a magical slumber. Roll 5d8: the total is how many hit points of creatures this spell can affect. Creatures within 20 feet of a point you choose within range are affected in ascending order of their current hit points (ignoring unconscious creatures).\n\nStarting with the creature that has the lowest current hit points, each creature affected by this spell falls unconscious until the spell ends, the sleeper takes damage, or someone uses an action to shake or slap the sleeper awake. Subtract each creatures's hit points from the total before moving on to the creature with the next lowest hit points. A creature's hit points must be equal to or less than the remaining total for that creature to be affected.\n\nUndead and creatures immune to being charmed aren't affected by this spell."
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
    , spDescription = "As you cast this spell, you use the rope to create a circle with a 5-foot radius on the ground or the floor. When you finish casting, the rope disappears and the circle becomes a magic trap.\n\nThis trap is nearly invisible, requiring a successful Intelligence (Investigation) check against your spell save DC to be discerned.\n\nThe trap triggers when a Small, Medium, or Large creature moves onto the ground or the floor in the spell's radius. That creature must succeed on a Dexterity saving throw or be magically hoisted into the air, leaving it hanging upside down 3 feet above the ground or floor. The creature is restrained there until the spell ends.\n\nA restrained creature can make a Dexterity saving throw at the end of each of its turns, ending the effect on iteself on a success. Alternatively, the creature or someone else who can reach it can use an action to make an Intelligence (Arcana) check against your spell save DC. On a success, the restrained effect ends.\n\nAfter the trap is triggered, the spell ends when no creature is restrained by it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Tashas Caustic Brew"
    , spLevel = 1
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
    , spLevel = 1
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (tiny tarts and a feather that is waved in the air)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A creature of your choice that you can see within range perceives everything as hilariously funny and falls into fits of laughter if this spell affects it. The target must succeed on a Wisdom saving throw or fall prone, becoming incapacitated and unable to stand up for the duration. A creature with an intelligence score of 4 or less isn't affected.\n\nAt the end of each of its turns, and each time it takes damage, the target can make another Wisdom saving throw. The target has advantage on the saving throw if it's triggered by damage. On success, the spell ends."
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
    , spDescription = "This spell creates a circular, horizontal plane of force, 3 feet in diameter and 1 inch thick, that floats 3 feet above groud in an unoccupiied space of your choice that you can see within range. The disk remains for the duration, and can hold up to 500 pounds. If more weight is placed on it, the spell ends, and everything on the disk falls to the ground.\n\nThe disk is immobile while you are within 20 feet of it. If you move more than 20 feet away from it, the disk follows you so that it remains within 20 feet of you. It can move across uneven terrain, up or down stairs, slopes and the like, but it can't cross an elevation change of 10 feet or more. For example, the disk can't move across a 10-foot-deep pit, nor could it leave such a pit if it was created at the bottom.\n\nIf you move more than 100 feet from the disk (typically because it can't move around an obstacle to follow you), the spell ends."
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
    , spDescription = "A wave of thunderous force sweeps out from you. Each creature in a 15-foot cube originating from you must make a Constitution saving throw. On a failed save, a creature takes 2d8 thunder damage and is pushed 10 feet away from you. On a successful save, the creature takes half as much damage and isn't pushed.\n\nIn addition, unsecured objects that are completely within the area of effect are automatically pushed 10 feet away from you by the spell's effect, and the spell emits a thunderous boom audible out to 300 feet."
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
    , spDescription = "This spell creates an invisible, mindless, shapeless force that performs simple tasks at your command until the spell ends. The servant springs into existence in an unoccupied space on the ground within range. It has AC 10, 1 hit point, and a Strength of 2, and it can't attack. If it drops to 0 hit points, the spell ends.\n\nOnce on each of your turns as a bonus action, you can mentally command the servant to move up to 15 feet and interact with an object. The servant can perform simple tasks that a human servant would do, such as fetching things, cleaning, mending, folding clothes, lighting fires, serving food, and pouring wine. Once you give the command, the servant performs the task to the best of its ability until it completes the task, then waits for your next command.\n\nIf you command the servant to perform a task that would move it more than 60 feet away from you, the spell ends."
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
  ]