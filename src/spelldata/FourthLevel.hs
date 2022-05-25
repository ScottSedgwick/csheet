{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}
module FourthLevel where

import SpellTypes

fourthLevel :: [Spell]
fourthLevel =
  [ Spell
    { spName = "Arcane Eye"
    , spLevel = 4
    , spType = Divination
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a bit of bat fur)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You create an invisible, magical eye within range that hovers in the air for the duration.\n\n" <>
      "You mentally receive visual information from the eye, which has normal vision and *darkvision* out to 30 feet. The eye can look in every direction.\n\n" <>
      "As an action, you can move the eye up to 30 feet in any direction. There is no limit to how far away from you the eye can move, but it can't enter another plane of existence. A solid barrier blocks the eye's movement, but the eye can pass through an opening as small as 1 inch in diameter."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Conj. Minor Elementals"
    , spLevel = 4
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 minute"
    , spRange = "90 feet"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You summon elementals that appear in unoccupied spaces that you can see within range. You choose one the following options for what appears:\n\n" <>
      "- One elemental of challenge rating 2 or lower\n\n" <>
      "- Two elemental of challenge rating 1 or lower\n\n" <>
      "- Four elemental of challenge rating 1/2 or lower\n\n" <>
      "- Eight elemental of challenge rating 1/4 or lower\n\n" <>
      "An elemental summoned by this spell disappears when it drops to 0 hit points or when the spell ends.\n\n" <>
      "The summoned creatures are friendly to you and your companions. Roll initiative for the summoned creatures as a group, which has its own turns. They obey any verbal commands that you issue to them (no action required by you). If you don't issue any commands to them, they defend themselves from hostile creatures, but otherwise take no actions.\n\n" <>
      "The GM has the creatures' statistics.\n\n" <>
      "| CR  | Monster |\n" <>
      "| --- | ------- |\n" <>
      "| 1/4 | Steam Mephit |\n" <>
      "| 1/2 | Dust Mephit, Ice Mephit, Magma Mephit, Magmin |\n" <>
      "| 2   | Azer, Gargoyle |"
    , spHigher = Just "When you cast this spell using certain higher-level spell slots, you choose one of the summoning options above, and more creatures appear: twice as many with a 6th-level slot and three times as many with an 8th-level slot."
    }
  , Spell
    { spName = "Divination"
    , spLevel = 4
    , spType = Conjuration
    , spRitual = True
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (incense and a sacrificial offering appropriate to your religion, together worth at least 25 gp, which the spell consumes)"
    , spDuration = "Instantaneous"
    , spDescription = "Your magic and an offering put you in contact with a god or a god's servants. You ask a single question concerning a specific goal, event, or activity to occur within 7 days. The GM offers a truthful reply. The reply might be a short phrase, a cryptic rhyme, or an omen.\n\n"
      <> "The spell doesn't take into account any possible circumstances that might change the outcome, such as the casting of additional spells or the loss or gain of a companion.\n\n"
      <> "If you cast the spell two or more times before finishing your next long rest, there is a cumulative 25 percent chance for each casting after the first that you get a random reading. The GM makes this roll in secret."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Elemental Bane"
    , spLevel = 4
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
    , spLevel = 4
    , spType = Transmutation
    , spRitual = False
    , spTime = "10 minutes"
    , spRange = "120 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You convert raw materials into products of the same material. For example, you can fabricate a wooden bridge from a clump of trees, a rope from a patch of hemp, and clothes from flax or wool.\n\n"
      <> "Choose raw materials that you can see within range. You can fabricate a Large or smaller object (contained within a 10-foot cube, or eight connected 5-foot cubes), given a sufficient quantity of raw material. If you are working with metal, stone, or another mineral substance, however, the fabricated object can be no larger than Medium (contained within a single 5-foot cube). The quality of objects made by the spell is commensurate with the quality of the raw materials.\n\n"
      <> "Creatures or magic items can't be created or transmuted by this spell. You also can't use it to create items that ordinarily require a high degree of craftsmanship, such as jewelry, weapons, glass, or armor, unless you have proficiency with the type of artisan's tools used to craft such objects."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Find Greater Steed"
    , spLevel = 4
    , spType = Conjuration
    , spRitual = False
    , spTime = "10 minutes"
    , spRange = "30 feet"
    , spComponents = "V,S"
    , spDuration = "Instantaneous"
    , spDescription = "You summon a spirit that assumes the form of a loyal, majestic mount. Appearing in an unoccupied space within range, the spirit takes on a form you choose: a griffon, a pegasus, a peryton, a dire wolf, a rhinoceros, or a saber-toothed tiger. The creature has the statistics provided in the Monster Manual for the chosen form, though it is a celestial, a fey, or a fiend (your choice) instead of its normal creature type. Additionally, if it has an Intelligence score of 5 or lower, its Intelligence becomes 6, and it gains the ability to understand one language of your choice that you speak.\n\n"
      <> "You control the mount in combat. While the mount is within 1 mile of you, you can communicate with it telepathically. While mounted on it, you can make any spell you cast that targets only you also target the mount.\n\n"
      <> "The mount disappears temporarily when it drops to 0 hit points or when you dismiss it as an action. Casting this spell again re-summons the bonded mount, with all its hit points restored and any conditions removed.\n\n"
      <> "You can't have more than one mount bonded by this spell or find steed at the same time. As an action, you can release a mount from its bond, causing it to disappear permanently.\n\n"
      <> "Whenever the mount disappears, it leaves behind any objects it was wearing or carrying."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Fire Shield"
    , spLevel = 4
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Self"
    , spComponents = "V,S,M (a bit of phosphorus or a firefly)"
    , spDuration = "10 minutes"
    , spDescription = "Thin and wispy flames wreathe your body for the duration, shedding bright light in a 10-foot radius and dim light for an additional 10 feet. You can end the spell early by using an action to dismiss it.\n\n"
      <> "The flames provide you with a warm shield or a chill shield, as you choose. The warm shield grants you resistance to cold damage, and the chill shield grants you resistance to fire damage.\n\n"
      <> "In addition, whenever a creature within 5 feet of you hits you with a melee attack, the shield erupts with flame. The attacker takes 2d8 fire damage from a warm shield, or 2d8 cold damage from a cold shield."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Freedom of Movement"
    , spLevel = 4
    , spType = Abjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (a leather strap, bound around the arm or a similar appendage)"
    , spDuration = "1 hour"
    , spDescription = "You touch a willing creature. For the duration, the target's movement is unaffected by difficult terrain, and spells and other magical effects can neither reduce the target's speed nor cause the target to be paralyzed or restrained.\n\n"
      <> "The target can also spend 5 feet of movement to automatically escape from nonmagical restraints, such as manacles or a creature that has it grappled. Finally, being underwater imposes no penalties on the target's movement or attacks."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Greater Invisibility"
    , spLevel = 4
    , spType = Illusion
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "You or a creature you touch becomes *invisible* until the spell ends. Anything the target is wearing or carrying is *invisible* as long as it is on the target's person."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Leomunds Secret Chest"
    , spLevel = 4
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "Touch"
    , spComponents = "V,S,M (an exquisite chest, 3 feet by 2 feet by 2 feet, constructed from rare materials worth at least 5,000 gp, and a Tiny replica made from the same materials worth at least 50 gp)"
    , spDuration = "Instantaneous"
    , spDescription = "You hide a chest, and all its contents, on the Ethereal Plane. You must touch the chest and the miniature replica that serves as a material component for the spell. The chest can contain up to 12 cubic feet of nonliving material (3 feet by 2 feet by 2 feet).\n\n"
      <> "While the chest remains on the Ethereal Plane, you can use an action and touch the replica to recall the chest. It appears in an unoccupied space on the ground within 5 feet of you. You can send the chest back to the Ethereal Plane by using an action and touching both the chest and the replica.\n\n"
      <> "After 60 days, there is a cumulative 5 percent chance per day that the spell’s effect ends. This effect ends if you cast this spell again, if the smaller replica chest is destroyed, or if you choose to end the spell as an action. If the spell ends and the larger chest is on the Ethereal Plane, it is irretrievably lost."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Faithful Hound"
    , spLevel = 4
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a tiny silver whistle, a piece of bone, and a thread)"
    , spDuration = "8 hours"
    , spDescription = "You conjure a phantom watchdog in an unoccupied space that you can see within range, where it remains for the duration, until you dismiss it as an action, or until you move more than 100 feet away from it.\n\n"
      <> "The hound is invisible to all creatures except you and can't be harmed. When a Small or larger creature comes within 30 feet of it without first speaking the password that you specify when you cast this spell, the hound starts barking loudly. The hound sees invisible creatures and can see into the Ethereal Plane. It ignores illusions.\n\n"
      <> "At the start of each of your turns, the hound attempts to bite one creature within 5 feet of it that is hostile to you. The hound's attack bonus is equal to your spellcasting ability modifier + your proficiency bonus. On a hit, it deals 4d8 piercing damage."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Private Sanctum"
    , spLevel = 4
    , spType = Abjuration
    , spRitual = False
    , spTime = "10 minutes"
    , spRange = "120 feet"
    , spComponents = "V,S,M (a thin sheet of lead, a piece of opaque glass, a wad of cotton or cloth, and powdered chrysolite)"
    , spDuration = "24 hours"
    , spDescription = "You make an area within range magically secure. The area is a cube that can be as small as 5 feet to as large as 100 feet on each side. The spell lasts for the duration or until you use an action to dismiss it.\n\n"
      <> "When you cast the spell, you decide what sort of security the spell provides, choosing any or all of the following properties:\n\n"
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
    , spLevel = 4
    , spType = Evocation
    , spRitual = False
    , spTime = "1 action"
    , spRange = "30 feet"
    , spComponents = "V,S,M (a hemispherical piece of clear crystal and a matching hemispherical piece of gum arabic)"
    , spDuration = "Concentration, up to 1 minute"
    , spDescription = "A sphere of shimmering force encloses a creature or object of Large size or smaller within range. An unwilling creature must make a Dexterity saving throw. On a failed save, the creature is enclosed for the duration.\n\n"
      <> "Nothing—not physical objects, energy, or other spell effects—can pass through the barrier, in or out, though a creature in the sphere can breathe there. The sphere is immune to all damage, and a creature or object inside can’t be damaged by attacks or effects originating from outside, nor can a creature inside the sphere damage anything outside it.\n\n"
      <> "The sphere is weightless and just large enough to contain the creature or object inside. An enclosed creature can use its action to push against the sphere’s walls and thus roll the sphere at up to half the creature’s speed. Similarly, the globe can be picked up and moved by other creatures.\n\n"
      <> "A disintegrate spell targeting the globe destroys it without harming anything inside it."
    , spHigher = Nothing
    }
  , Spell
    { spName = "Stone Shape"
    , spLevel = 4
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
    , spLevel = 4
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
    , spLevel = 4
    , spType = Conjuration
    , spRitual = False
    , spTime = "1 action"
    , spRange = "90 feet"
    , spComponents = "V,S,M (an ornate stone and metal lockbox worth at least 400 gp)"
    , spDuration = "Concentration, up to 1 hour"
    , spDescription = "You call forth the spirit of a construct. It manifests in an unoccupied space that you can see within range. This corporeal form uses the Construct Spirit stat block. When you cast the spell, choose a material: Clay, Metal, or Stone. The creature resembles a golem or a modron (your choice) made of the chosen material, which determines certain traits in its stat block. The creature disappears when it drops to 0 hit points or when the spell ends.\n\n"
      <> "The creature is an ally to you and your companions. In combat, the creature shares your initiative count, but it takes its turn immediately after yours. It obeys your verbal commands (no action required by you). If you don't issue any, it takes the Dodge action and uses its move to avoid danger.\n\n"
      <> "##### Construct Spirit\n"
      <> "*Medium Construct*\n\n"
      <> "**Armour Class:** 13 + the level of the spell (natural armour)\n\n"
      <> "**Hit Points:** 40 + 15 for each spell level above 4th\n\n"
      <> "**Speed:** 30 ft.\n\n"
      <> "| STR | DEX | CON | INT | WIS | CHA |\n"
      <> "| --- | --- | --- | --- | --- | --- |\n"
      <> "| 18 (+4) | 10 (+0) | 18 (+4) | 14 (+2) | 11 (+0) | 5 (-3) |\n"
      <> "**Damage Resistances:** poison\n\n"
      <> "**Condition Immunities:** charmed, exhaustion, frightened, incapacitated, paralyzed, petrified, poisoned\n\n"
      <> "**Senses:** darkvision 60 ft., passive Perception 10\n\n"
      <> "**Languages:** understands the languages you speak\n\n"
      <> "**Challenge — Proficiency Bonus** equals your bonus\n\n"
      <> "***Heated Body (Metal Only).*** A creature that touches the construct or hits it with a melee attack while within 5 feet of it takes 1d10 fire damage.\n\n"
      <> "***Stony Lethargy (Stone Only).*** When a creature the construct can see starts its turn within 10 feet of the construct, the construct can force it to make a Wisdom saving throw against your spell save DC. On a failed save, the target can’t use reactions and its speed is halved until the start of its next turn.\n\n"
      <> "##### Actions\n\n"
      <> "***Multiattack.*** The construct makes a number of attacks equal to half this spell's level (rounded down).\n\n"
      <> "***Slam.*** Melee Weapon Attack: your spell attack modifier to hit, reach 5 ft., one target. Hit: 1d8 + 4 + the spell's level bludgeoning damage.\n\n"
      <> "##### Reactions\n\n"
      <> "***Berserk Lashing (Clay Only).*** When the construct takes damage, it makes a slam attack against a random creature within 5 feet of it. If no creature is within reach, the construct moves up to half its speed toward an enemy it can see, without provoking opportunity attacks."
    , spHigher = Just "When you cast this spell using a spell slot of 4th level or higher, use the higher level wherever the spell's level appears in the stat block."
    }
  ]