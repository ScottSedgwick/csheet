{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module FifthLevel where

import SpellTypes

fifthLevel :: [Spell]
fifthLevel =
  [ Spell
    { spName = "Animate Objects"
    , spLevel = 5
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "Objects come to life at your command. Choose up to ten nonmagical objects within range that are not being worn or carried. Medium targets count as two objects, Large targets count as four objects, Huge targets count as eight objects. You can't animate any object larger than Huge. Each target animates and becomes a creature under your control until the spell ends or until reduced to 0 hit points.\n\n" <>
      "As a bonus action, you can mentally command any creature you made with this spell if the creature is within 500 feet of you (if you control multiple creatures, you can command any or all of them at the same time, issuing the same command to each one). You decide what action the creature will take and where it will move during its next turn, or you can issue a general command, such as to guard a particular chamber or corridor. If you issue no commands, the creature only defends itself against hostile creatures. Once given an order, the creature continues to follow it until its task is complete.\n\n" <>
      "##### Animated Object Statistics\n\n" <>
      "| Size   | HP  | AC  | Str | Dex | Attack |\n" <>
      "| ------ | --- | --- | --- | --- | ------ |\n" <>
      "| Tiny   | 20  | 18  | 4   | 18  | +8 to hit, 1d4+4 damage  |\n" <>
      "| Small  | 25  | 16  | 6   | 14  | +6 to hit, 1d8+2 damage  |\n" <>
      "| Medium | 40  | 13  | 10  | 12  | +5 to hit, 2d6+1 damage  |\n" <>
      "| Large  | 50  | 10  | 14  | 10  | +6 to hit, 2d10+2 damage |\n" <>
      "| Huge   | 80  | 10  | 18  | 6   | +8 to hit, 2d12+4 damage |\n\n" <>
      "An animated object is a construct with AC, hit points, attacks, Strength, and Dexterity determined by its size. Its Constitution is 10 and its Intelligence and Wisdom are 3, and its Charisma is 1. Its speed is 30 feet; if the object lacks legs or other appendages it can use for locomotion, it instead has a flying speed of 30 feet and can hover. If the object is securely attached to a surface or a larger object, such as a chain bolted to a wall, its speed is 0. It has *blindsight* with a radius of 30 feet and is blind beyond that distance. When the animated object drops to 0 hit points, it reverts to its original object form, and any remaining damage carries over to its original object form.\n\n" <>
      "If you command an object to attack, it can make a single melee attack against a creature within 5 feet of it. It makes a slam attack with an attack bonus and bludgeoning damage determined by its size. The GM might rule that a specific object inflicts slashing or piercing damage based on its form."
    , spHigher = Just "If you cast this spell using a spell slot of 6th level or higher, you can animate two additional objects for each slot level above 5th."
    }
  , Spell
    { spName = "Bigbys Hand"
    , spLevel = 5
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (an eggshell and a snakeskin glove)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You create a Large hand of shimmering, translucent force in an unoccupied space that you can see within range. The hand lasts for the spell's duration, and it moves at your command, mimicking the movements of your own hand.\n\n" <>
      "The hand is an object that has AC 20 and hit points equal to your hit point maximum. If it drops to 0 hit points, the spell ends. It has a Strength of 26 (+8) and a Dexterity of 10 (+0). The hand doesn't fill its space.\n\n" <>
      "When you cast the spell and as a bonus action on your subsequent turns, you can move the hand up to 60 feet and then cause one of the following effects with it.\n\n" <>
      "***Clenched Fist.*** The hand strikes one creature or object within 5 feet of it. Make a melee spell attack for the hand using your game statistics. On a hit, the target takes 4d8 force damage.\n\n" <>
      "***Forceful Hand.*** The hand attempts to push a creature within 5 feet of it in a direction you choose. Make a check with the hand's Strength contested by the Strength (*Athletics*) check of the target. If the target is Medium or smaller, you have advantage on the check. If you succeed, the hand pushes the target up to 5 feet plus a number of feet equal to five times your spellcasting ability modifier. The hand moves with the target to remain within 5 feet of it.\n\n" <>
      "***Grasping Hand.*** The hand attempts to grapple a Huge or smaller creature within 5 feet of it. You use the hand's Strength score to resolve the grapple. If the target is Medium or smaller, you have advantage on the check. While the hand is grappling the target, you can use a bonus action to have the hand crush it. When you do so, the target takes bludgeoning damage equal to 2d6 + your spellcasting ability modifier.\n\n" <>
      "***Interposing Hand.*** The hand interposes itself between you and a creature you choose until you give the hand a different command. The hand moves to stay between you and the target, providing you with half cover against the target. The target can't move through the hand's space if its Strength score is less than or equal to the hand's Strength score. If its Strength score is higher than the hand's Strength score, the target can move toward you through the hand's space, but that space is difficult terrain for the target."
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the damage from the clenched fist option increases by 2d8 and the damage from the grasping hand increases by 2d6 for each slot level above 5th."
    }
  , Spell
    { spName = "Cone of Cold"
    , spLevel = 5
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a small crystal or glass cone)"
    , spDuration = "Instantaneous"
    , spDescription = "A blast of cold air erupts from your hands. Each creature in a 60-foot cone must make a Constitution saving throw. A creature takes 8d8 cold damage on a failed save, or half as much damage on a successful one.\n\n" <>
      "A creature killed by this spell becomes a frozen statue until it thaws."
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the damage increases by 1d8 for each slot level above 5th."
    }
  , Spell
    { spName = "Conjure Elemental"
    , spLevel = 5
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "90 feet"
    , spComponents = "V,S,M (burning incense for air, soft clay for earth, sulphur and phosphorous for fire, or water and sand for water)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You call forth an elemental servant. Choose an area of air, earth, fire, or water that fills a 10-foot cube within range. An elemental of challenge rating 5 or lower appropriate to the area you chose appears in an unoccupied space within 10 feet of it. For example, a fire elemental emerges from a bonfire, and an earth elemental rises up from the ground. The elemental disappears when it drops to 0 hit points or when the spell ends.\n\n" <>
      "The elemental is friendly to you and your companions for the duration. Roll initiative for the elemental, which has its own turns. It obeys any verbal commands that you issue to it (no action required by you). If you don't issue any commands to the elemental, it defends itself from hostile creatures but otherwise takes no actions.\n\n" <>
      "If your concentration is broken, the elemental doesn't disappear. Instead, you lose control of the elemental, it becomes hostile toward you and your companions, and it might attack. An uncontrolled elemental can't be dismissed by you, and it disappears 1 hour after you summoned it.\n\n" <>
      "The GM has the elemental's statistics. Sample elementals can be found below.\n\n" <>
      "##### Sample Elementals\n\n" <>
      "| CR  | Creature Name |\n" <>
      "| --- | ------------- |\n" <>
      "| 1/4 | Steam Mephit  |\n" <>
      "| 1/2 | Dust Mephit, Ice Mephit, Magma Mephit, Magmin |\n" <>
      "| 2   | Azer, Gargoyle  |\n" <>
      "| 5   | Air Elemental, Earth Elemental, Fire Elemental, Salamander, Water Elemental, Xorn |\n" <>
      "| 6   | Invisible Stalker  |"
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the challenge rating increases by 1 for each slot level above 5th."
    }
  , Spell
    { spName = "Creation"
    , spLevel = 5
    , spType = Illusion
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a tiny piece of matter of the same type of the item you plan to create)"
    , spDuration = "Special"
    , spDescription = "You pull wisps of shadow material from the Shadowfell to create a nonliving object of vegetable matter within range:  soft goods, rope, wood, or something similar. You can also use this spell to create mineral objects such as stone, crystal, or metal. The object created must be no larger than a 5-foot cube, and the object must be of a form and material that you have seen before.\n\n"
      <> "The duration depends on the object's material. If the object is composed of multiple materials, use the shortest duration.\n\n"
      <> "| Material              | Duration |\n"
      <> "| --------------------- | -------- |\n"
      <> "| Vegetable matter      | 1 day    |\n"
      <> "| Stone or crystal      | 1 day    |\n"
      <> "| Precious metals       | 1 day    |\n"
      <> "| Gems                  | 1 day    |\n"
      <> "| Adamantine or mithral | 1 day    |\n\n"
      <> "Using any material created by this spell as another spell's material component causes that spell to fail."
    , spHigher = Just "When you cast this spell using a spell slot of 6th level or higher, the cube increases by 5 feet for each slot level above 5th."
    }
  , Spell
    { spName = "Greater Restoration"
    , spLevel = 5
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (diamond dust worth at least 100 gp, which the spell consumes)"
    , spDuration = "Instantaneous"
    , spDescription = "You imbue a creature you touch with positive energy to undo a debilitating effect. You can reduce the target's exhaustion level by one, or end one of the following effects on the target:\n\n"
      <> "- One effect that charmed or petrified the target\n"
      <> "- One curse, including the target's attunement to a cursed magic item\n"
      <> "- Any reduction to one of the target's ability scores\n"
      <> "- One effect reducing the target's hit point maximum"
    , spHigher = Nothing
    }
  , Spell
    { spName = "Passwall"
    , spLevel = 5
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a pinch of sesame seeds)"
    , spDuration = "1 hour"
    , spDescription = "A passage appears at a point of your choice that you can see on a wooden, plaster, or stone surface (such as a wall, a ceiling, or a floor) within range, and lasts for the duration. You choose the opening's dimensions: up to 5 feet wide, 8 feet tall, and 20 feet deep. The passage creates no instability in a structure surrounding it.\n\n"
      <> "When the opening disappears, any creatures or objects still in the passage created by the spell are safely ejected to an unoccupied space nearest to the surface on which you cast the spell."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Skill Empowerment"
    , spLevel = 5
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "Your magic deepens a creatures understanding of its own talent. You touch one willing creature and give it expertise in one skill of your choice; until the spell ends, the creature doubles its proficiency bonus for ability checks it makes that use the chosen skill.\n\n"
      <> "You must choose a skill in which the target is proficient and that isnt already benefiting from an effect, such as Expertise, that doubles its proficiency bonus."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Transmute Rock"
    , spLevel = 5
    , spType = Transmutation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (clay and water)"
    , spDuration = "Until Dispelled"
    , spDescription = "You choose an area of stone or mud that you can see that fits within a 40-foot cube and is within range, and choose one of the following effects.\n\n"
      <> "***Transmute Rock to Mud.*** Nonmagical rock of any sort in the area becomes an equal volume of thick, flowing mud that remains for the spell's duration.\n\n"
      <> "The ground in the spell's area becomes muddy enough that creatures can sink into it. Each foot that a creature moves through the mud costs 4 feet of movement, and any creature on the ground when you cast the spell must make a Strength saving throw. A creature must also make the saving throw when it moves into the area for the first time on a turn or ends its turn there. On a failed save, a creature sinks into the mud and is restrained, though it can use an action to end the restrained condition on itself by pulling itself free of the mud.\n\n"
      <> "If you cast the spell on a ceiling, the mud falls. Any creature under the mud when it falls must make a Dexterity saving throw. A creature takes 4d8 bludgeoning damage on a failed save, or half as much damage on a successful one.\n\n"
      <> "***Transmute Mud to Rock.*** Nonmagical mud or quicksand in the area no more than 10 feet deep transforms into soft stone for the spell's duration. Any creature in the mud when it transforms must make a Dexterity saving throw. On a successful save, a creature is shunted safely to the surface in an unoccupied space. On a failed save, a creature becomes restrained by the rock. A restrained creature, or another creature within reach, can use an action to try to break the rock by succeeding on a DC 20 Strength check or by dealing damage to it. The rock has AC 15 and 25 hit points, and it is immune to poison and psychic damage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Wall of Force"
    , spLevel = 5
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a pinch of powder made by crushing a clear gemstone)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "An invisible wall of force springs into existence at a point you choose within range. The wall appears in any orientation you choose, as a horizontal or vertical barrier or at an angle. It can be free floating or resting on a solid surface. You can form it into a hemispherical dome or a sphere with a radius of up to 10 feet, or you can shape a flat surface made up of ten 10-foot-by-10-foot panels. Each panel must be contiguous with another panel. In any form, the wall is 1/4 inch thick. It lasts for the duration. If the wall cuts through a creature's space when it appears, the creature is pushed to one side of the wall (your choice which side).\n\n"
      <> "Nothing can physically pass through the wall. It is immune to all damage and can't be dispelled by dispel magic. A disintegrate spell destroys the wall instantly, however. The wall also extends into the Ethereal Plane, blocking ethereal travel through the wall."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Wall of Stone"
    , spLevel = 5
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a small block of granite)"
    , spDuration = "Concentration, up to 10 minutes"
    , spDescription = "A nonmagical wall of solid stone springs into existence at a point you choose within range. The wall is 6 inches thick and is composed of ten 10-foot- by-10-foot panels. Each panel must be contiguous with at least one other panel. Alternatively, you can create 10-foot-by-20-foot panels that are only 3 inches thick.\n\n"
      <> "If the wall cuts through a creature's space when it appears, the creature is pushed to one side of the wall (your choice). If a creature would be surrounded on all sides by the wall (or the wall and another solid surface), that creature can make a Dexterity saving throw. On a success, it can use its reaction to move up to its speed so that it is no longer enclosed by the wall.\n\n"
      <> "The wall can have any shape you desire, though it can't occupy the same space as a creature or object. The wall doesn't need to be vertical or rest on any firm foundation. It must, however, merge with and be solidly supported by existing stone. Thus, you can use this spell to bridge a chasm or create a ramp.\n\n"
      <> "If you create a span greater than 20 feet in length, you must halve the size of each panel to create supports. You can crudely shape the wall to create crenellations, battlements, and so on.\n\n"
      <> "The wall is an object made of stone that can be damaged and thus breached. Each panel has AC 15 and 30 hit points per inch of thickness. Reducing a panel to 0 hit points destroys it and might cause connected panels to collapse at the GM's discretion.\n\n"
      <> "If you maintain your concentration on this spell for its whole duration, the wall becomes permanent and can't be dispelled. Otherwise, the wall disappears when the spell ends."
    , spHigher = Nothing
    }
  ]