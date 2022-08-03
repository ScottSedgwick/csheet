{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module ThirdLevel where

import SpellTypes

thirdLevel :: [Spell]
thirdLevel =
  [ Spell
    { spName = "Blink"
    , spLevel = 3
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S"
    , spDuration = "1 minute"
    , spDescription = "Roll a d20 at the end of each of your turns for the duration of the spell. On a roll of 11 or higher, you vanish from your current plane of existence and appear in the Ethereal Plane (the spell fails and the casting is wasted if you were already on that plane). At the start of your next turn, and when the spell ends if you are on the Ethereal Plane, you return to an unoccupied space of your choice that you can see within 10 feet of the space you vanished from. If no unoccupied space is available within that range, you appear in the nearest unoccupied space (chosen at random if more than one space is equally near). You can dismiss this spell as an action.\n\n" <>
      "While on the Ethereal Plane, you can see and hear the plane you originated from, which is cast in shades of gray, and you can't see anything there more than 60 feet away. You can only affect and be affected by other creatures on the Ethereal Plane. Creatures that aren't there can't perceive you or interact with you, unless they have the ability to do so."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Call Lightning"
    , spLevel = 3
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "A storm cloud appears in the shape of a cylinder that is 10 feet tall with a 60-foot radius, centered on a point you can see within range directly above you. The spell fails if you can’t see a point in the air where the storm cloud could appear (for example, if you are in a room that can’t accommodate the cloud).\n\n" <>
      "When you cast the spell, choose a point you can see under the cloud. A bolt of lightning flashes down from the cloud to that point. Each creature within 5 feet of that point must make a Dexterity saving throw. A creature takes 3d10 lightning damage on a failed save, or half as much damage on a successful one. On each of your turns until the spell ends, you can use your action to call down lightning in this way again, targeting the same point or a different one.\n\n" <>
      "If you are outdoors in stormy conditions when you cast this spell, the spell gives you control over the existing storm instead of creating a new one. Under such conditions, the spell’s damage increases by 1d10."
    , spHigher = Just "When you cast this spell using a spell slot of 4th or higher level, the damage increases by 1d10 for each slot level above 3rd."
    }
  , Spell
    { spName = "Catnap"
    , spLevel = 3
    , spType = Enchantment
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "S,M (a pinch of sand)"
    , spDuration = "10 minutes"
    , spDescription = "You make a calming gesture, and up to three willing creatures of your choice that you can see within range fall *unconscious* for the spell's duration. The spell ends on a target early if it takes damage or someone uses an action to shake or slap it awake. If a target remains *unconscious* for the full duration, that target gains the benefit of a short rest, and it can't be affected by this spell again until it finishes a long rest."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, you can target one additional willing creature for each slot level above 3rd."
    }
  , Spell
    { spName = "Create Food and Water"
    , spLevel = 3
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
    , spLevel = 3
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
    , spLevel = 3
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
    , spLevel = 3
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "150 feet"
    , spComponents = "V,S,M (a tiny ball of bat guano and sulfur)"
    , spDuration = "Instantaneous"
    , spDescription = "A bright streak flashes from your pointing finger to a point you choose within range and then blossoms with a low roar into an explosion of flame. Each creature in a 20-foot-radius sphere centered on that point must make a Dexterity saving throw. A target takes 8d6 fire damage on a failed save, or half as much damage on a successful one.\n\n"
      <> "The fire spreads around corners. It ignites flammable objects in the area that aren't being worn or carried."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, the damage increases by 1d6 for each slot level above 3rd."
    }
  , Spell
    { spName = "Flame Arrows"
    , spLevel = 3
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
    , spLevel = 3
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
    , spLevel = 3
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 hour"
    , spRange = "Touch"
    , spComponents = "V,S,M (incense and powdered diamond worth at least 200 gp, which the spell consumes)"
    , spDuration = "Until Dispelled or Triggered"
    , spDescription = "When you cast this spell, you inscribe a glyph that later unleashes a magical effect. You inscribe it either on a surface (such as a table or a section of floor or wall) or within an object that can be closed (such as a book, a scroll, or a treasure chest) to conceal the glyph. The glyph can cover an area no larger than 10 feet in diameter. If the surface or object is moved more than 10 feet from where you cast this spell, the glyph is broken, and the spell ends without being triggered.\n\n"
      <> "The glyph is nearly invisible and requires a successful Intelligence (Investigation) check against your spell save DC to be found.\n\n"
      <> "You decide what triggers the glyph when you cast the spell. For glyphs inscribed on a surface, the most typical triggers include touching or standing on the glyph, removing another object covering the glyph, approaching within a certain distance of the glyph, or manipulating the object on which the glyph is inscribed. For glyphs inscribed within an object, the most common triggers include opening that object, approaching within a certain distance of the object, or seeing or reading the glyph. Once a glyph is triggered, this spell ends.\n\n"
      <> "You can further refine the trigger so the spell activates only under certain circumstances or according to physical characteristics (such as height or weight), creature kind (for example, the ward could be set to affect aberrations or drow), or alignment. You can also set conditions for creatures that don’t trigger the glyph, such as those who say a certain password.\n\n"
      <> "When you inscribe the glyph, choose explosive runes or a spell glyph.\n\n"
      <> "***Explosive Runes.*** When triggered, the glyph erupts with magical energy in a 20-foot-radius sphere centered on the glyph. The sphere spreads around corners. Each creature in the area must make a Dexterity saving throw. A creature takes 5d8 acid, cold, fire, lightning, or thunder damage on a failed saving throw (your choice when you create the glyph), or half as much damage on a successful one.\n\n"
      <> "***Spell Glyph.*** You can store a prepared spell of 3rd level or lower in the glyph by casting it as part of creating the glyph. The spell must target a single creature or an area. The spell being stored has no immediate effect when cast in this way. When the glyph is triggered, the stored spell is cast. If the spell has a target, it targets the creature that triggered the glyph. If the spell affects an area, the area is centered on that creature. If the spell summons hostile creatures or creates harmful objects or traps, they appear as close as possible to the intruder and attack it. If the spell requires concentration, it lasts until the end of its full duration."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, the damage of an *explosive runes* glyph increases by 1d8 for each slot level above 3rd. If you create a *spell glyph*, you can store any spell of up to the same level as the slot you use for the glyph of warding."
    }
  , Spell
    { spName = "Haste"
    , spLevel = 3
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a shaving of licorice root)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose a willing creature that you can see within range. Until the spell ends, the target's speed is doubled, it gains a +2 bonus to AC, it has advantage on Dexterity saving throws, and it gains an additional action on each of its turns. That action can be used only to take the Attack (one weapon attack only), Dash, Disengage, Hide, or Use an Object action.\n\n"
      <> "When the spell ends, the target can't move or take actions until after its next turn, as a wave of lethargy sweeps over it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Hypnotic Pattern"
    , spLevel = 3
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "S,M (a glowing stick of incense or a crystal vial filled with phosphorescent material)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You create a twisting pattern of colors that weaves through the air inside a 30-foot cube within range. The pattern appears for a moment and vanishes. Each creature in the area who sees the pattern must make a Wisdom saving throw. On a failed save, the creature becomes charmed for the duration. While charmed by this spell, the creature is incapacitated and has a speed of 0.\n\n"
      <> "The spell ends for an affected creature if it takes any damage or if someone else uses an action to shake the creature out of its stupor."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Intellect Fortress"
    , spLevel = 3
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
    , spLevel = 3
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
    , spLevel = 3
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
    , spLevel = 3
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a drop of molasses)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You alter time around up to six creatures of your choice in a 40-foot cube within range. Each target must succeed on a Wisdom saving throw or be affected by this spell for the duration.\n\n"
      <> "An affected target's speed is halved, it takes a -2 penalty to AC and Dexterity saving throws, and it can't use reactions. On its turn, it can use either an action or a bonus action, not both. Regardless of the creature's abilities or magic items, it can't make more than one melee or ranged attack during its turn.\n\n"
      <> "If the creature attempts to cast a spell with a casting time of 1 action, roll a d20. On an 11 or higher, the spell doesn't take effect until the creature's next turn, and the creature must use its action on that turn to complete the spell. If it can't, the spell is wasted.\n\n"
      <> "A creature affected by this spell makes another Wisdom saving throw at the end of each of its turns. On a successful save, the effect ends for it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Tiny Servant"
    , spLevel = 3
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "8 hours"
    , spDescription = "You touch one Tiny, nonmagical object that isnt attached to another object or a surface and isnt being carried by another creature. The target animates and sprouts little arms and legs, becoming a creature under your control until the spell ends or the creature drops to 0 hit points. See Tiny Servant for its statistics.\n\n"
      <> "As a bonus action, you can mentally command the creature if it is within 120 feet of you. (If you control multiple creatures with this spell, you can command any or all of them at the same time, issuing the same command to each one.) You decide what action the creature will take and where it will move during its next turn, or you can issue a simple, general command, such as to fetch a key, stand watch, or stack some books. If you issue no commands, the servant does nothing other than defend itself against hostile creatures. Once given an order, the servant continues to follow that order until its task is complete.\n\n"
      <> "When the creature drops to 0 hit points, it reverts to its original form, and any remaining damage carries over to that form."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, you can animate two additional objects for each slot level above 3rd."
    }
  , Spell
    { spName = "Water Breathing"
    , spLevel = 3
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
    , spLevel = 3
    , spType = Transmutation
    , spRitual = True
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a piece of cork)"
    , spDuration = "1 hour"
    , spDescription = "This spell grants the ability to move across any liquid surface--such as water, acid, mud, snow, quicksand, or lava--as if it were harmless solid ground (creatures crossing molten lava can still take damage from the heat). Up to ten willing creatures you can see within range gain this ability for the duration.\n\n"
      <> "If you target a creature submerged in a liquid, the spell carries the target to the surface of the liquid at a rate of 60 feet per round."
    , spHigher = Nothing
    }
  ]