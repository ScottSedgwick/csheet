{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module SecondLevel where

import SpellTypes

secondLevel :: [Spell]
secondLevel =
  [ Spell
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
    , spDescription = "You assume a different form. When you cast the spell, choose one of the following options, the effects of which last for the duration of the spell. While the spell lasts, you can end one option as an action to gain the benefits of a different one.\n\n***Aquatic Adaptation.*** You adapt your body to an aquatic environment, sprouting gills and growing webbing between your fingers. You can breathe underwater and gain a swimming speed equal to your walking speed.\n\n***Change Appearance.*** You transform your appearance. You decide what you look like, including your height, weight, facial features, sound of your voice, hair length, coloration, and distinguishing characteristics, if any. You can make yourself appear as a member of another race, though none of your statistics change. You also can't appear as a creature of a different size than you, and your basic shape stays the same; if you're bipedal, you can't use this spell to become quadrupedal, for instance. At any time for the duration of the spell, you can use your action to change your appearance in this way again.\n\n***Natural Weapons.*** You grow claws, fangs, spines, horns, or a different natural weapon of your choice. Your unarmed strikes deal 1d6 bludgeoning, piercing, or slashing damage, as appropriate to the natural weapon you chose, and you are proficient with your unarmed strikes. Finally, the natural weapon is magic and you have a +1 bonus to the attack and damage rolls you make using it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Aid"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (gold dust worth at least 25gp, which the spell consumes)"
    , spDuration = "Until dispelled"
    , spDescription = "You touch a closed door, window, gate, chest, or other entryway, and it becomes locked for the duration. You and the creatures you designate when you cast this spell can open the object normally. You can also set a password that, when spoken within 5 feet of the object, suppresses this spell for 1 minute. Otherwise, it is impassable until it is broken or the spell is dispelled or suppressed. Casting *knock* on the object suppresses *arcane lock* for 10 minutes. \n\n"
      <> "While affected by this spell, the object is more difficult to break or force open; the DC to break it or pick any locks on it increases by 10."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Arcanists Magic Aura"
    , spLevel = 2
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a small square of silk)"
    , spDuration = "24 Hours"
    , spDescription = "You place an illusion on a creature or an object you touch so that divination spells reveal false information about it. The target can be a willing creature or an object that isn't being carried or worn by another creature.\n\nWhen you cast the spell, choose one or both of the following effects. The effect lasts for the duration. If you cast this spell on the same creature or object every day for 30 days, placing the same effect on it each time, the illusion lasts until it is dispelled.\n\n"
      <> "**False Aura.** You change the way the target appears to spells and magical effects, such as *detect magic*, that detect magical auras. You can make a nonmagical object appear magical, a magical object appear nonmagical, or change the object's magical aura so that it appears to belong to a specific school of magic that you choose. When you use this effect on an object, you can make the false magic apparent to any creature that handles the item.\n\n"
      <> "**Mask.** You change the way the target appears to spells and magical effects that detect creature types, such as a paladin's Divine Sense or the trigger of a *symbol* spell. You choose a creature type and other spells and magical effects treat the target as if it were a creature of that type or of that alignment."
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
    , spDescription = "You can blind or deafen a foe. Choose one creature that you can see within range to make a Constitution saving throw. If it fails, the target is either *blinded* or *deafened* (your choice) for the duration. At the end of each of its turns, the target can make a Constitution saving throw. On a success, the spell ends."
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
    , spDescription = "Your body becomes blurred, shifting and wavering to all who can see you. For the duration, any creature has disadvantage on attack rolls against you. An attacker is immune to this effect if it doesn't rely on sight, as with *blindsight*, or can see through illusions, as with *truesight*."
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
    , spDescription = "A flame, equivalent in brightness to a torch, springs forth from an object that you touch. The effect looks like a regular flame, but it creates no heat and doesn't use oxygen. A *continual flame* can be covered or hidden but not smothered or quenched."
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
    , spDescription = "One humanoid of your choice that you can see within range must succeed on a Wisdom saving throw or become charmed by you for the duration. While the target is charmed in this way, a twisted crown of jagged iron appears on its head, and madness glows in its eyes.\n\nThe charmed target must use its action before moving on each of its turns to make a melee attack against a creature other than itself that you mentally choose. The target can act normally on its turn if you choose no creature or if none are within reach.\n\nOn your subsequent turns, you must use your action to maintain control over the target, or the spell ends. Also, the target can make a Wisdom saving throw at the end of each of its turns. An a success, the spell ends."
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
    , spDescription = "Magical darkness spreads from a point you choose within range to fill a 15-foot-radius sphere for the duration. The darkness spreads around corners. A creature with **darkvision** can't see through this darkness, and nonmagical light can't illuminate it.\n\nIf the point you choose is on an object you are holding or one that isn't being worn or carried, the darkness emanates from the object and moves with it. Completely covering the source of the darkness with an opaque object, such as a bowl or a helm, blocks the darkness.\n\nIf any of this spell's area overlaps with an area of light created by a spell of 2nd level or lower, the spell that created the light is dispelled."
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
    , spDescription = "You touch a willing creature to grant it the ability to see in the dark. For the duration, that creature has **darkvision** out to a range of 60 feet."
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
    , spDescription = "For the duration, you can read the thoughts of certain creatures. When you cast the spell and as your action on each turn until the spell ends, you can focus your mind on any one creature that you can see within 30 feet of you. If the creature you choose has an Intelligence of 3 or lower or doesn't speak any language, the creature is unaffected.\n\nYou initially learn the surface thoughts of the creature--what is most on its mind in that moment. As an action, you can either shift your attention to another creature's thoughts or attempt to probe deeper into the same creature's mind. If you probe deeper, the target must make a Wisdom saving throw. If it fails, you gain insight into its reasoning (if any), its emotional state, and something that looms large in its mind (such as something it worries over, loves, or hates). If it succeeds, the spell ends. Either way, the target knows that you are probing into its mind, and unless you shift your attention to another creature's thoughts, the creature can use its action on its turn to make an Intelligence check contested by your Intelligence check; if it succeeds, the spell ends.\n\nQuestions verbally directed at the target creature naturally shape the course of its thoughts, so this spell is particularly effective as part of an interrogation.\n\nYou can also use this spell to detect the presence of thinking creatures you can't see. When you cast the spell or as your action during the duration, you can search for thoughts within 30 feet of you. The spell can penetrate barriers, but 2 feet of rock, 2 inches of any metal other than lead, or a thin sheet of lead blocks you. You can't detect a creature with an Intelligence of 3 or lower or one that doesn't speak any language.\n\nOnce you detect the presence of a creature in this way, you can read its thoughts for the rest of the duration as described above, even if you can't see it, but it must still be within range."
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
    , spDescription = "Choose an unoccupied 5-foot cube of air that you can see within range. An elemental force that resembles a dust devil appears in the cube and lasts for the spell’s duration.\n\nAny creature that ends its turn within 5 feet of the dust devil must make a Strength saving throw. On a failed save, the creature takes 1d8 bludgeoning damage and is pushed 10 feet away. On a successful save, the creature takes half as much damage and isn’t pushed.\n\nAs a bonus action, you can move the dust devil up to 30 feet in any direction. If the dust devil moves over sand, dust, loose dirt, or small gravel, it sucks up the material and forms a 10-foot-radius cloud of debris around itself that lasts until the start of your next turn. The cloud heavily obscures its area."
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
    { spName = "Enhance Ability"
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (fur or feather from a beast)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You touch a creature and bestow upon it a magical enhancement. Choose one of the following effects; the target gains that effect until the spell ends.\n\n"
      <> "***Bear's Endurance.*** The target has advantage on Constitution checks. It also gains 2d6 temporary hit points, which are lost when the spell ends.\n\n"
      <> "***Bull's Strength.*** The target has advantage on Strength checks, and his or her carrying capacity doubles.\n\n"
      <> "***Cat's Grace.*** The target has advantage on Dexterity checks. It also doesn't take damage from falling 20 feet or less if it isn't incapacitated.\n\n"
      <> "***Eagle's Splendor.*** The target has advantage on Charisma checks.\n\n"
      <> "***Fox's Cunning.*** The target has advantage on Intelligence checks.\n\n"
      <> "***Owl's Wisdom.*** The target has advantage on Wisdom checks"
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, you can target one additional creature for each slot level above 2nd."
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
    , spDescription = "You cause a creature or an object you can see within range to grow larger or smaller for the duration. Choose either a creature or an object that is neither worn nor carried. If the target is unwilling, it can make a Constitution saving throw. On a success, the spell has no effect.\n\nIf the target is a creature, everything it is wearing and carrying changes size with it. Any item dropped by an affected creature returns to normal size at once.\n\n"
      <> "**Enlarge.** The target's size doubles in all dimensions, and its weight is multiplied by eight. This growth increases its size by one category - from Medium to Large, for example. If there isn't enough room for the target to double its size, the creature or object attains the maximum possible size in the space available. Until the spell ends, the target also has advantage on Strength checks and Strength saving throws. The target's weapons also grow to match its new size. While these weapons are enlarged, the target's attacks with them deal 1d4 extra damage.\n\n"
      <> "**Reduce.** The target's size is halved in all dimensions, and its weight is reduced to one-eighth of normal. This reduction decreases its size by one category - from Medium to Small, for example. Until the spell ends, the target also has disadvantage on Strength checks and Strength saving throws. The target's weapons also shrink to match its new size. While these weapons are reduced, the target's attacks with them deal 1d4 less damage (this can't reduce the damage below 1)."
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
    , spDescription = "A 5-foot-diameter sphere of fire appears in an unoccupied space of your choice within range and lasts for the duration. Any creature that ends its turn within 5 feet of the sphere must make a Dexterity saving throw. The creature takes 2d6 fire damage on a failed save, or half as much damage on a successful one.\n\nAs a bonus action, you can move the sphere up to 30 feet. If you ram the sphere into a creature, that creature must make the saving throw against the sphere's damage, and the sphere stops moving this turn.\n\nWhen you move the sphere, you can direct it over barriers up to 5 feet tall and jump it across pits up to 10 feet wide. The sphere ignites flammable objects not being worn or carried, and it sheds bright light in a 20-foot radius and dim light for an additional 20 feet."
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
    , spDescription = "You touch a corpse or other remains. For the duration, the target is protected from decay and can't become undead.\n\nThe spell also effectively extends the time limit on raising the target from the dead, since days spent under the influence of this spell don't count against the time limit of spells such as **raise dead**."
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
    , spDescription = "A line of strong wind 60 feet long and 10 feet wide blasts from you in a direction you choose for the spell's duration. Each creature that starts its turn in the line must succeed on a Strength saving throw or be pushed 15 feet away from you in a direction following the line.\n\nAny creature in the line must spend 2 feet of movement for every 1 foot it moves when moving closer to you.\n\nThe gust disperses gas or vapor, and it extinguishes candles, torches, and similar unprotected flames in the area. It causes protected flames, such as those of lanterns, to dance wildly and has a 50 percent chance to extinguish them.\n\nAs a bonus action on each of your turns before the spell ends, you can change the direction in which the line blasts from you."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Heat Metal"
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (a piece of iron and a flame)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Choose a manufactured metal object, such as a metal weapon or a suit of heavy or medium metal armor, that you can see within range. You cause the object to glow red-hot. Any creature in physical contact with the object takes 2d8 fire damage when you cast the spell. Until the spell ends, you can use a bonus action on each of your subsequent turns to cause this damage again.\n\n"
      <> "If a creature is holding or wearing the object and takes the damage from it, the creature must succeed on a Constitution saving throw or drop the object if it can. If it doesn't drop the object, it has disadvantage on attack rolls and ability checks until the start of your next turn."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
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
    , spDescription = "Choose a humanoid that you can see within range. The target must succeed on a Wisdom saving throw or be **paralyzed** for the duration. At the end of each of its turns, the target can make another Wisdom saving throw. On a success, the spell ends on the target."
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
    , spDescription = "A creature you touch becomes **invisible** until the spell ends. Anything the target is wearing or carrying is **invisible** as long as it is on the target's person. The spell ends for a target that attacks or casts a spell."
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
    , spDescription = "Choose an object that you can see within range. The object can be a door, a box, a chest, a set of manacles, a padlock, or another object that contains a mundane or magical means that prevents access.\n\nA target that is held shut by a mundane lock or that is stuck or barred becomes unlocked, unstuck, or unbarred. If the object has multiple locks, only one of them is unlocked.\n\nIf you choose a target that is held shut with *arcane lock*, that spell is suppressed for 10 minutes, during which time the target can be opened and shut normally.\n\nWhen you cast the spell, a loud knock, audible from as far away as 300 feet, emanates from the target object."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Lesser Restoration"
    , spLevel = 2
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
    , spLevel = 2
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "60 feet"
    , spComponents = "V,S,M (either a small leather loop or a piece of golden wire bent into a cup shape with a long shank on one end)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "One creature or loose object of your choice that you can see within range rises vertically, up to 20 feet, and remains suspended there for the duration. The spell can levitate a target that weighs up to 500 pounds. An unwilling creature that succeeds on a Constitution saving throw is unaffected.\n\nThe target can move only by pushing or pulling against a fixed object or surface within reach (such as a wall or a ceiling), which allows it to move as if it were climbing. You can change the target's altitude by up to 20 feet in either direction on your turn. If you are the target, you can move up or down as part of your move. Otherwise, you can use your action to move the target, which must remain within the spell's range.\n\nWhen the spell ends, the target floats gently to the ground if it is still aloft."
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
    , spDescription = "Describe or name an object that is familiar to you. You sense the direction to the object's location, as long as that object is within 1,000 feet of you. If the object is in motion, you know the direction of its movement.\n\nThe spell can locate a specific object known to you, as long as you have seen it up close--within 30 feet--at least once. Alternatively, the spell can locate the nearest object of a particular kind, such as a certain kind of apparel, jewelry, furniture, tool, or weapon.\n\nThis spell can't locate an object if any thickness of lead, even a thin sheet, blocks a direct path between you and the object."
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
    , spDescription = "You implant a message within an object in range, a message that is uttered when a trigger condition is met. Choose an object that you can see and that isn't being worn or carried by another creature. Then speak the message, which must be 25 words or less, though it can be delivered over as long as 10 minutes. Finally, determine the circumstance that will trigger the spell to deliver your message.\n\nWhen that circumstance occurs, a magical mouth appears on the object and recites the message in your voice and at the same volume you spoke. If the object you chose has a mouth or something that looks like a mouth (for example, the mouth of a statue), the magical mouth appears there so that the words appear to come from the object's mouth. When you cast this spell, you can have the spell end after it delivers its message, or it can remain and repeat its message whenever the trigger occurs.\n\nThe triggering circumstance can be as general or as detailed as you like, though it must be based on visual or audible conditions that occur within 30 feet of the object. For example, you could instruct the mouth to speak when any creature moves within 30 feet of the object or when a silver bell rings within 30 feet of it."
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
    , spDescription = "You choose a 5-foot-square unoccupied space on the ground that you can see within range. A Medium hand made from compacted soil rises there and reaches for one creature you can see within 5 feet of it. The target must make a Strength saving throw. On a failed save, the target takes 2d6 bludgeoning damage and is **restrained** for the spell’s duration.\n\nAs an action, you can cause the hand to crush the **restrained** target, who must make a Strength saving throw. It takes 2d6 bludgeoning damage on a failed save, or half as much damage on a successful one.\n\nTo break out, the **restrained** target can use its action to make a Strength check against your spell save DC. On a success, the target escapes and is no longer **restrained** by the hand.\n\nAs an action, you can cause the hand to reach for a different creature or to move to a different unoccupied space within range. The hand releases a **restrained** target if you do either."
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
    , spDescription = "Three illusory duplicates of yourself appear in your space. Until the spell ends, the duplicates move with you and mimic your actions, shifting position so it's impossible to track which image is real. You can use your action to dismiss the illusory duplicates.\n\n"
      <> "Each time a creature targets you with an attack during the spell's duration, roll a d20 to determine whether the attack instead targets one of your duplicates.\n\nIf you have three duplicates, you must roll a 6 or higher to change the attack's target to a duplicate. With two duplicates, you must roll an 8 or higher. With one duplicate, you must roll an 11 or higher.\n\n"
      <> "A duplicate's AC equals 10 + your Dexterity modifier. If an attack hits a duplicate, the duplicate is destroyed. A duplicate can be destroyed only by an attack that hits it. It ignores all other damage and effects. The spell ends when all three duplicates are destroyed.\n\n"
      <> "A creature is unaffected by this spell if it can't see, if it relies on senses other than sight, such as **blindsight**, or if it can perceive illusions as false, as with **truesight**."
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
    , spDescription = "You place an illusion on a creature or an object you touch so that divination spells reveal false information about it. The target can be a willing creature or an object that isn't being carried or worn by another creature.\n\nWhen you cast the spell, choose one or both of the following effects. The effect lasts for the duration. If you cast this spell on the same creature or object every day for 30 days, placing the same effect on it each time, the illusion lasts until it is dispelled.\n\n**False Aura.** You change the way the target appears to spells and magical effects, such as *detect magic*, that detect magical auras. You can make a nonmagical object appear magical, a magical object appear nonmagical, or change the object's magical aura so that it appears to belong to a specific school of magic that you choose. When you use this effect on an object, you can make the false magic apparent to any creature that handles the item.\n\n**Mask.** You change the way the target appears to spells and magical effects that detect creature types, such as a paladin's Divine Sense or the trigger of a *symbol* spell. You choose a creature type and other spells and magical effects treat the target as if it were a creature of that type or of that alignment."
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
    , spDescription = "You craft an illusion that takes root in the mind of a creature that you can see within range. The target must make an Intelligence saving throw. On a failed save, you create a phantasmal object, creature, or other visible phenomenon of your choice that is no larger than a 10-foot cube and that is perceivable only to the target for the duration. The spell has no effect on undead or constructs.\n\nThe phantasm includes sound, temperature and other stimuli, also evident only to the creature.\n\nThe target can use its action to examine the phantasm as if it were real. The target rationalizes any illogical outcomes from interacting with the phantasm. For example, a target attempting to walk across a phantasmal bridge that spans a chasm falls once it steps on to the bridge. If the target survives the fall, it still believes that the bridge exists and comes up with some other explanation for its fall - it was pushed, it slipped, or a strong wind might have knocked it off.\n\nAn affected target is so convinced of the phantasm's reality that it can even take damage from the illusion. A phantasm created to appear as fire, a pool of acid, or lava can burn the target. Each round on your turn, the phantasm can deal 1d6 psychic damage to the target if it is in the phantasm's area or within 5 feet of the phantasm, provided that the illusion is of a creature or hazard that could logically deal damage, such as by attacking. The target perceives the damage as a type appropriate to the illusion."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Protection from Poison"
    , spLevel = 2
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "1 hour"
    , spDescription = "You touch a creature. If it is poisoned, you neutralize the poison. If more than one poison afflicts the target, you neutralize one poison that you know is present, or you neutralize one at random.\n\n"
      <> "For the duration, the target has advantage on saving throws against being poisoned, and it has resistance to poison damage."
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
    , spDescription = "Choose an area of nonmagical flame that you can see and that fits within a 5-foot cube within range. You can extinguish the fire in that area, and you create either fireworks or smoke when you do so.\n\n"
      <> "**Fireworks.** The target explodes with a dazzling display of colors. Each creature within 10 feet of the target must succeed on a Constitution saving throw or become **blinded** until the end of your next turn.\n\n"
      <> "**Smoke.** Thick black smoke spreads out from the target in a 20-foot radius, moving around corners. The area of the smoke is heavily obscured. The smoke persists for 1 minute or until a strong wind disperses it."
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
    , spDescription = "A black beam of enervating energy springs from your finger toward a creature within range. Make a ranged spell attack against the target. On a hit, the target deals only half damage with weapon attacks that use Strength until the spell ends.\n\nAt the end of each of the target's turns, it can make a Constitution saving throw against the spell. On a success, the spell ends."
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
    , spDescription = "You touch a length of rope that is up to 60 feet long. One end of the rope then rises into the air until the whole rope hangs perpendicular to the ground. At the upper end of the rope, an invisible entrance opens to an extradimensional space that lasts until the spell ends.\n\nThe extradimensional space can be reached by climbing to the top of the rope. The space can hold as many as eight Medium or smaller creatures. The rope can be pulled into the space, making the rope disappear from view outside the space.\n\nAttacks and spells can't cross through the entrance into or out of the extradimensional space, but those inside can see out of it as if through a 3-foot-by-5- foot window centered on the rope.\n\nAnything inside the extradimensional space drops out when the spell ends."
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
    , spDescription = "You create three rays of fire and hurl them at targets within range. You can hurl them at one target or several.\n\nMake a ranged spell attack for each ray. On a hit, the target takes 2d6 fire damage."
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
    , spDescription = "For the duration, you see **invisible** creatures and objects as if they were visible, and you can see into the Ethereal Plane. Ethereal creatures and objects appear ghostly and translucent."
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
    , spDescription = "You weave together threads of shadow to create a sword of solidified gloom in your hand. The magic sword lasts until the spell ends. It counts as a simple melee weapon with which you are proficient. It deals 2d8 psychic damage on a hit and has the finesse, light and thrown properties (range 20/60). In addition, when you use the sword to attack a target that is in dim light or darkness, you make the attack roll with advantage.\n\nIf you drop the weapon or throw it, it dissapates at the end of the turn. Thereafter, while the spell persists, you can use your bonus action to cause the sword to reappear in your hand."
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
    , spDescription = "A sudden loud ringing noise, painfully intense, erupts from a point of your choice within range. Each creature in a 10-foot-radius sphere centered on that point must make a Constitution saving throw. A creature takes 3d8 thunder damage on a failed save, or half as much damage on a successful one. A creature made of inorganic material such as stone, crystal, or metal has disadvantage on this saving throw.\n\nA nonmagical object that isn't being worn or carried also takes the damage if it's in the spell's area."
    , spHigher = Just "When you cast this spell using a spell slot of 3rd level or higher, the damage increases by 1d8 for each slot level above 2nd."
    }
  , Spell
    { spName = "Silence"
    , spLevel = 2
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
    , spDescription = "You suggest a course of activity (limited to a sentence or two) and magically influence a creature you can see within range that can hear and understand you. Creatures that can't be **charmed** are immune to this effect. The suggestion must be worded in such a manner as to make the course of action sound reasonable. Asking the creature to stab itself, throw itself onto a spear, immolate itself, or do some other obviously harmful act ends the spell.\n\nThe target must make a Wisdom saving throw. On a failed save, it pursues the course of action you described to the best of its ability. The suggested course of action can continue for the entire duration. If the suggested activity can be completed in a shorter time, the spell ends when the subject finishes what it was asked to do.\n\nYou can also specify conditions that will trigger a special activity during the duration. For example, you might suggest that a knight give her warhorse to the first beggar she meets. If the condition isn't met before the spell expires, the activity isn't performed.\n\nIf you or any of your companions damage the target, the spell ends."
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
    , spDescription = "A strong wind (20 miles per hour) blows around you in a 10-foot radius and moves with you, remaining centered on you. The wind lasts for the spell’s duration.\n\nThe wind has the following effects:\n* It deafens you and other creatures in its area.\n* It extinguishes unprotected flames in its area that are torch-sized or smaller.\n* It hedges out vapor, gas, and fog that can be dispersed by strong wind.\n* The area is difficult terrain for creatures other than you.\n* The attack rolls of ranged weapon attacks have disadvantage if the attacks pass in or out of the wind."
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
    , spDescription = "You conjure a mass of thick, sticky webbing at a point of your choice within range. The webs fill a 20-foot cube from that point for the duration. The webs are difficult terrain and lightly obscure their area.\n\nIf the webs aren't anchored between two solid masses (such as walls or trees) or layered across a floor, wall, or ceiling, the conjured web collapses on itself, and the spell ends at the start of your next turn. Webs layered over a flat surface have a depth of 5 feet.\n\nEach creature that starts its turn in the webs or that enters them during its turn must make a Dexterity saving throw. On a failed save, the creature is **restrained** as long as it remains in the webs or until it breaks free.\n\nA creature **restrained** by the webs can use its action to make a Strength check against your spell save DC. If it succeeds, it is no longer restrained.\n\nThe webs are flammable. Any 5-foot cube of webs exposed to fire burns away in 1 round, dealing 2d4 fire damage to any creature that starts its turn in the fire."
    , spHigher = Nothing
    }
  ]