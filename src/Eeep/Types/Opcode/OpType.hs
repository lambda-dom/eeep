{- |
Module: Eeep.Types.Opcode.OpType

The @OpType@ type.
-}

module Eeep.Types.Opcode.OpType (
    -- * Types.
    OpType (..),

    -- * Basic functions.
    toOpType,
    fromOpType,
) where

-- Imports.
-- Base.
import Data.Word (Word32)
import Data.Ix (Ix)

-- Libraries.
import Data.Vector.Strict (Vector, (!), fromList, force)
import Data.Maybe (isJust)


{- | The @OpType@ enumeration type for all IE opcodes up to BG2 EE. -}
data OpType
    = ACDamageType
    | AttacksRound
    | CureSleep
    | Berserk
    | CureBerserk
    | CharmSpecific
    | Charisma
    | ColorCharacterPalette
    | ColorRGB
    | ColorGlowPulse
    | Constitution
    | CurePoison
    | Damage
    | DeathInstant
    | Defrost
    | Dexterity
    | Haste
    | HPCurrent
    | HPMaximum
    | Intelligence
    | Invisibility
    | Lore
    | Luck
    | Morale
    | Horror
    | Poison
    | RemoveCursed
    | AcidResistance
    | ColdResitance
    | ElectricityResistance
    | FireResistance
    | MagicDamageResistance
    | CureDeath
    | SavesDeath
    | SavesWands
    | SavesPolymorph
    | SavesBreath
    | SavesSpells
    | Silence
    | Unconscious
    | Slow
    | Sparkle
    | WizardSpellSlots
    | CurePetrification
    | Strength
    | Stun
    | CureStun
    | CureInvisibilityAlternative
    | CureSilence
    | Wisdom
    | ColorGlowRGB
    | ColorDarkRGB
    | ColorBrightRGB
    | AnimationChange
    | THAC0
    | DeathCreatureType
    | AlignmentInvert
    | AlignmentChange
    | CureDispellable
    | Stealth
    | MiscastMagic
    | ColorFadeRGB
    | PriestSpellSlots
    | Infravision
    | RemoveInfravision
    | Blur
    | TransparencyFade
    | SummonCreature
    | UnsummonCreature
    | ImmunityDetection
    | CureImmunityDetection
    | ChangeSex
    | SetIDSState
    | ExtraDamage
    | Blindness
    | CureBlindness
    | Feebleminded
    | CureFeebleminded
    | Disease
    | CureDisease
    | Deafness
    | CureDeafness
    | SetAIScript
    | ImmunityProjectile
    | MagicFireResistance
    | MagicColdResistance
    | SlashingResistance
    | CrushingResistance
    | PiercinggResistance
    | MissileResistance
    | OpenLocks
    | FindTraps
    | PickPockets
    | Fatigue
    | Drunkenness
    | Tracking
    | Level
    | ExceptionalStrength
    | HPRegeneration
    | SpellEffectDuration
    | ImmunityCreatureType
    | ImmunityOpcode
    | SpellImmunityPower
    | ChangeName
    | Experience
    | Gold
    | MoraleBreak
    | ChangePortrait
    | Reputation
    | Paralyze
    | CreateMagicWeapon
    | RemoveItem
    | Dither
    | DetectAlignment
    | CureInvisibility
    | RevealArea
    | MirrorImage
    | ProtectionFromWeapons
    | CreateInventoryItem
    | RemoveInventoryItem
    | Teleport
    | Unlock
    | Movement
    | SummonMonster
    | Confusion
    | Aid
    | Bless
    | PositiveChant
    | RaisePhysicalStats
    | LuckNonCumulative
    | Petrification
    | PolymorphSpecific
    | ForceVisible
    | NegativeChant
    | CharacterAnimationChange
    | DisplayString
    | CastingGlow
    | LightingEffect
    | DisplayIcon
    | CreateItemSlot
    | DisableButton
    | DisableCasting
    | CastSpellCreature
    | LearnSpell
    | CastSpellPoint
    | EffectFindTraps
    | ReplaceCreature
    | PlayMovie
    | Sanctuary
    | Entangle
    | Globe
    | ProtectionMissiles
    | Web
    | Grease
    | EffectMirrorImage
    | RemoveSanctuary
    | CureHorror
    | CureParalyze
    | FreeAction
    | CureDrunkenness
    | EffectPauseTarget
    | MagicResistance
    | THAC0Missile
    | RemoveCreature
    | ImmunityIcon
    | DamageAnimation
    | LearnAbility
    | RemoveSpell
    | PoisonResistance
    | PlaySoundEffect
    | Hold
    | MovementUnconditioned
    | UseEff
    | THAC0CreatureType
    | DamageCreatureType
    | CannotUseItem
    | CannotUseItemType
    | ItemApplyEffect
    | ItemTypeApplyEffect
    | Passwall
    | HoldHelpless
    | MoveToArea
    | StoreLocalVariable
    | AuraCleanse
    | CastingTime
    | AttackSpeed
    | CastingLevel
    | FindFamiliar
    | TargetInvisible
    | IgnoreDialogPause
    | FamiliarBond
    | FamiliarBlock
    | BounceSpellProjectile
    | BounceSpellOpcode
    | BounceSpellPower
    | BounceSpellPowerDecrement
    | ImmunityPowerDecrement
    | BounceSpellSchool
    | BounceSpellSectype
    | ImmunitySpellSchool
    | ImmunitySpellSectype
    | ImmunitySpell
    | SpellBounceResource
    | HPMinimumLimit
    | DeathKill60
    | SpellStun90
    | Imprisonment
    | Freedom
    | Maze
    | SelectSpell
    | Play3DEffect
    | LevelDrain
    | Unconscious20
    | Stoneskin
    | ACCreatureType
    | RemoveSchool
    | RemoveSectype
    | TeleportField
    | ImmunitySchoolDecrement
    | CureLevelDrain
    | RevealMagic
    | ImmunitySectypeDecrement
    | BounceSpellSchoolDecrement
    | BounceSpellSectypeDecrement
    | RemoveSchoolOne
    | RemoveSectypeOne
    | TimeStop
    | CastSpellCondition
    | WeaponProficiency
    | CreateContingency
    | WingBuffet
    | ImageProjection
    | SpellPuppetID
    | DeathDisintegrate
    | Farsight
    | RemoveIcon
    | ControlCreature
    | CureConfusion
    | DrainCharges
    | DrainWizardSpell
    | CheckedBerserk
    | EffectBerserk
    | AttackNearest
    | SetMeleeEffect
    | SetRangedEffect
    | LuckDamage
    | ChangeBardSong
    | SetTrap
    | AddMapMarker
    | RemoveMapMarker
    | CreateInventoryItemWithDays
    | CreateSpellSequencer
    | CreateCustomSpellSequencer
    | ActivateSpellSequencerCreature
    | TrapPowerDecrement
    | ActivateSpellSequencerPoint
    | RestoreSpells
    | VisualRange
    | Backstab
    | DropWeapons
    | ModifyGlobalVariable
    | RemoveImmunitySpell
    | ImmunityDisplayString
    | ClearFog
    | ShakeWindow
    | CurePauseTarget
    | RemoveAvatar
    | RepeatingEff
    | RemoveAreaProjectile
    | TeleportToTarget
    | HideInShadows
    | DetectIllusion
    | SetTraps
    | ModifiedTHAC0
    | EnableButton
    | EffectWildMagic
    | WildMagic
    | ScriptState
    | UseEffCursed
    | MeleeTHAC0
    | MeleeDamage
    | RangedDamage
    | RemoveSelectionCircle
    | FistTHAC0
    | FistDamage
    | ChangeTitle
    | ImmunityVisualEffect
    | ImmunityBackstab
    | EnableOffScreenAI
    | SpellExistenceDelay
    | DisablePermanentDeath
    | ImmunitySpecificAnimation
    | ImmunityTurnUndead
    | TeleportPocketPlane
    | ChaosShield
    | NPCBump
    | CriticalHit
    | CanUseAnyItem
    | Assassinate
    | MassRaiseDead
    | THAC0Offhand
    | THAC0Onhand
    | AbilityTracking
    | ImmunityTracking
    | ModifyLocalVariable
    | ImmunityTimeStop
    | RandomWish
    | ImmunitySequester
    | GainedHLA
    | GolemStoneskin
    | AnimationRemoval
    | MagicRest
    | HasteUnconditioned
    | ImmunityResource
    | ItemUsability
    | ChangeWeather
    | RemoveEffectsResource
    | TurnUndeadLevel
    | ImmunityResourceMessage
    | SavesAll
    | ApplyEffectsList
    | VisualSpellHit
    | SetState
    | SlowPoison
    | FloatText
    | SummonRandomMonster
    | SpecificDamage
    | StaticCharge
    | TurnUndead
    | SevenEyes
    | DisplayEyes
    | RemoveOpcode
    | DisableRest
    | AlterAnimation
    | SetBackstabEffect
    | SetCriticalHitEffect
    | AnimationOverrideData
    | HPSwap
    | WeaponEnchantmentCreatureType
    | WeaponEnchantmentBonus
    | SavesSchoolBonus
    | IgnoreReputationBreak
    | SetCriticalMissEffect
    | CriticalMiss
    | ModalStateCheck
    | MakeUnselectable
    | EffectOnMove
    | MinimumBaseStats
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


{- | Enumeration of the opcode types for parsing and serialization. -}
optypes :: [(Word32, Maybe OpType)]
optypes = [
    (0,   Just ACDamageType),
    (1,   Just AttacksRound),
    (2,   Just CureSleep),
    (3,   Just Berserk),
    (4,   Just CureBerserk),
    (5,   Just CharmSpecific),
    (6,   Just Charisma),
    (7,   Just ColorCharacterPalette),
    (8,   Just ColorRGB),
    (9,   Just ColorGlowPulse),
    (10,  Just Constitution),
    (11,  Just CurePoison),
    (12,  Just Damage),
    (13,  Just DeathInstant),
    (14,  Just Defrost),
    (15,  Just Dexterity),
    (16,  Just Haste),
    (17,  Just HPCurrent),
    (18,  Just HPMaximum),
    (19,  Just Intelligence),
    (20,  Just Invisibility),
    (21,  Just Lore),
    (22,  Just Luck),
    (23,  Just Morale),
    (24,  Just Horror),
    (25,  Just Poison),
    (26,  Just RemoveCursed),
    (27,  Just AcidResistance),
    (28,  Just ColdResitance),
    (29,  Just ElectricityResistance),
    (30,  Just FireResistance),
    (31,  Just MagicDamageResistance),
    (32,  Just CureDeath),
    (33,  Just SavesDeath),
    (34,  Just SavesWands),
    (35,  Just SavesPolymorph),
    (36,  Just SavesBreath),
    (37,  Just SavesSpells),
    (38,  Just Silence),
    (39,  Just Unconscious),
    (40,  Just Slow),
    (41,  Just Sparkle),
    (42,  Just WizardSpellSlots),
    (43,  Just CurePetrification),
    (44,  Just Strength),
    (45,  Just Stun),
    (46,  Just CureStun),
    (47,  Just CureInvisibilityAlternative),
    (48,  Just CureSilence),
    (49,  Just Wisdom),
    (50,  Just ColorGlowRGB),
    (51,  Just ColorDarkRGB),
    (52,  Just ColorBrightRGB),
    (53,  Just AnimationChange),
    (54,  Just THAC0),
    (55,  Just DeathCreatureType),
    (56,  Just AlignmentInvert),
    (57,  Just AlignmentChange),
    (58,  Just CureDispellable),
    (59,  Just Stealth),
    (60,  Just MiscastMagic),
    (61,  Just ColorFadeRGB),
    (62,  Just PriestSpellSlots),
    (63,  Just Infravision),
    (64,  Just RemoveInfravision),
    (65,  Just Blur),
    (66,  Just TransparencyFade),
    (67,  Just SummonCreature),
    (68,  Just UnsummonCreature),
    (69,  Just ImmunityDetection),
    (70,  Just CureImmunityDetection),
    (71,  Just ChangeSex),
    (72,  Just SetIDSState),
    (73,  Just ExtraDamage),
    (74,  Just Blindness),
    (75,  Just CureBlindness),
    (76,  Just Feebleminded),
    (77,  Just CureFeebleminded),
    (78,  Just Disease),
    (79,  Just CureDisease),
    (80,  Just Deafness),
    (81,  Just CureDeafness),
    (82,  Just SetAIScript),
    (83,  Just ImmunityProjectile),
    (84,  Just MagicFireResistance),
    (85,  Just MagicColdResistance),
    (86,  Just SlashingResistance),
    (87,  Just CrushingResistance),
    (88,  Just PiercinggResistance),
    (89,  Just MissileResistance),
    (90,  Just OpenLocks),
    (91,  Just FindTraps),
    (92,  Just PickPockets),
    (93,  Just Fatigue),
    (94,  Just Drunkenness),
    (95,  Just Tracking),
    (96,  Just Level),
    (97,  Just ExceptionalStrength),
    (98,  Just HPRegeneration),
    (99,  Just SpellEffectDuration),
    (100, Just ImmunityCreatureType),
    (101, Just ImmunityOpcode),
    (102, Just SpellImmunityPower),
    (103, Just ChangeName),
    (104, Just Experience),
    (105, Just Gold),
    (106, Just MoraleBreak),
    (107, Just ChangePortrait),
    (108, Just Reputation),
    (109, Just Paralyze),
    (110, Nothing),
    (111, Just CreateMagicWeapon),
    (112, Just RemoveItem),
    (113, Nothing),
    (114, Just Dither),
    (115, Just DetectAlignment),
    (116, Just CureInvisibility),
    (117, Just RevealArea),
    (118, Nothing),
    (119, Just MirrorImage),
    (120, Just ProtectionFromWeapons),
    (121, Nothing),
    (122, Just CreateInventoryItem),
    (123, Just RemoveInventoryItem),
    (124, Just Teleport),
    (125, Just Unlock),
    (126, Just Movement),
    (127, Just SummonMonster),
    (128, Just Confusion),
    (129, Just Aid),
    (130, Just Bless),
    (131, Just PositiveChant),
    (132, Just RaisePhysicalStats),
    (133, Just LuckNonCumulative),
    (134, Just Petrification),
    (135, Just PolymorphSpecific),
    (136, Just ForceVisible),
    (137, Just NegativeChant),
    (138, Just CharacterAnimationChange),
    (139, Just DisplayString),
    (140, Just CastingGlow),
    (141, Just LightingEffect),
    (142, Just DisplayIcon),
    (143, Just CreateItemSlot),
    (144, Just DisableButton),
    (145, Just DisableCasting),
    (146, Just CastSpellCreature),
    (147, Just LearnSpell),
    (148, Just CastSpellPoint),
    (149, Nothing),
    (150, Just EffectFindTraps),
    (151, Just ReplaceCreature),
    (152, Just PlayMovie),
    (153, Just Sanctuary),
    (154, Just Entangle),
    (155, Just Globe),
    (156, Just ProtectionMissiles),
    (157, Just Web),
    (158, Just Grease),
    (159, Just EffectMirrorImage),
    (160, Just RemoveSanctuary),
    (161, Just CureHorror),
    (162, Just CureParalyze),
    (163, Just FreeAction),
    (164, Just CureDrunkenness),
    (165, Just EffectPauseTarget),
    (166, Just MagicResistance),
    (167, Just THAC0Missile),
    (168, Just RemoveCreature),
    (169, Just ImmunityIcon),
    (170, Just DamageAnimation),
    (171, Just LearnAbility),
    (172, Just RemoveSpell),
    (173, Just PoisonResistance),
    (174, Just PlaySoundEffect),
    (175, Just Hold),
    (176, Just MovementUnconditioned),
    (177, Just UseEff),
    (178, Just THAC0CreatureType),
    (179, Just DamageCreatureType),
    (180, Just CannotUseItem),
    (181, Just CannotUseItemType),
    (182, Just ItemApplyEffect),
    (183, Just ItemTypeApplyEffect),
    (184, Just Passwall),
    (185, Just HoldHelpless),
    (186, Just MoveToArea),
    (187, Just StoreLocalVariable),
    (188, Just AuraCleanse),
    (189, Just CastingTime),
    (190, Just AttackSpeed),
    (191, Just CastingLevel),
    (192, Just FindFamiliar),
    (193, Just TargetInvisible),
    (194, Just IgnoreDialogPause),
    (195, Just FamiliarBond),
    (196, Just FamiliarBlock),
    (197, Just BounceSpellProjectile),
    (198, Just BounceSpellOpcode),
    (199, Just BounceSpellPower),
    (200, Just BounceSpellPowerDecrement),
    (201, Just ImmunityPowerDecrement),
    (202, Just BounceSpellSchool),
    (203, Just BounceSpellSectype),
    (204, Just ImmunitySpellSchool),
    (205, Just ImmunitySpellSectype),
    (206, Just ImmunitySpell),
    (207, Just SpellBounceResource),
    (208, Just HPMinimumLimit),
    (209, Just DeathKill60),
    (210, Just SpellStun90),
    (211, Just Imprisonment),
    (212, Just Freedom),
    (213, Just Maze),
    (214, Just SelectSpell),
    (215, Just Play3DEffect),
    (216, Just LevelDrain),
    (217, Just Unconscious20),
    (218, Just Stoneskin),
    (219, Just ACCreatureType),
    (220, Just RemoveSchool),
    (221, Just RemoveSectype),
    (222, Just TeleportField),
    (223, Just ImmunitySchoolDecrement),
    (224, Just CureLevelDrain),
    (225, Just RevealMagic),
    (226, Just ImmunitySectypeDecrement),
    (227, Just BounceSpellSchoolDecrement),
    (228, Just BounceSpellSectypeDecrement),
    (229, Just RemoveSchoolOne),
    (230, Just RemoveSectypeOne),
    (231, Just TimeStop),
    (232, Just CastSpellCondition),
    (233, Just WeaponProficiency),
    (234, Just CreateContingency),
    (235, Just WingBuffet),
    (236, Just ImageProjection),
    (237, Just SpellPuppetID),
    (238, Just DeathDisintegrate),
    (239, Just Farsight),
    (240, Just RemoveIcon),
    (241, Just ControlCreature),
    (242, Just CureConfusion),
    (243, Just DrainCharges),
    (244, Just DrainWizardSpell),
    (245, Just CheckedBerserk),
    (246, Just EffectBerserk),
    (247, Just AttackNearest),
    (248, Just SetMeleeEffect),
    (249, Just SetRangedEffect),
    (250, Just LuckDamage),
    (251, Just ChangeBardSong),
    (252, Just SetTrap),
    (253, Just AddMapMarker),
    (254, Just RemoveMapMarker),
    (255, Just CreateInventoryItemWithDays),
    (256, Just CreateSpellSequencer),
    (257, Just CreateCustomSpellSequencer),
    (258, Just ActivateSpellSequencerCreature),
    (259, Just TrapPowerDecrement),
    (260, Just ActivateSpellSequencerPoint),
    (261, Just RestoreSpells),
    (262, Just VisualRange),
    (263, Just Backstab),
    (264, Just DropWeapons),
    (265, Just ModifyGlobalVariable),
    (266, Just RemoveImmunitySpell),
    (267, Just ImmunityDisplayString),
    (268, Just ClearFog),
    (269, Just ShakeWindow),
    (270, Just CurePauseTarget),
    (271, Just RemoveAvatar),
    (272, Just RepeatingEff),
    (273, Just RemoveAreaProjectile),
    (274, Just TeleportToTarget),
    (275, Just HideInShadows),
    (276, Just DetectIllusion),
    (277, Just SetTraps),
    (278, Just ModifiedTHAC0),
    (279, Just EnableButton),
    (280, Just EffectWildMagic),
    (281, Just WildMagic),
    (282, Just ScriptState),
    (283, Just UseEffCursed),
    (284, Just MeleeTHAC0),
    (285, Just MeleeDamage),
    (286, Just RangedDamage),
    (287, Just RemoveSelectionCircle),
    (288, Just FistTHAC0),
    (289, Just FistDamage),
    (290, Just ChangeTitle),
    (291, Just ImmunityVisualEffect),
    (292, Just ImmunityBackstab),
    (293, Just EnableOffScreenAI),
    (294, Just SpellExistenceDelay),
    (295, Just DisablePermanentDeath),
    (296, Just ImmunitySpecificAnimation),
    (297, Just ImmunityTurnUndead),
    (298, Just TeleportPocketPlane),
    (299, Just ChaosShield),
    (300, Just NPCBump),
    (301, Just CriticalHit),
    (302, Just CanUseAnyItem),
    (303, Just Assassinate),
    (304, Just MassRaiseDead),
    (305, Just THAC0Offhand),
    (306, Just THAC0Onhand),
    (307, Just AbilityTracking),
    (308, Just ImmunityTracking),
    (309, Just ModifyLocalVariable),
    (310, Just ImmunityTimeStop),
    (311, Just RandomWish),
    (312, Just ImmunitySequester),
    (313, Just GainedHLA),
    (314, Just GolemStoneskin),
    (315, Just AnimationRemoval),
    (316, Just MagicRest),
    (317, Just HasteUnconditioned),
    (318, Just ImmunityResource),
    (319, Just ItemUsability),
    (320, Just ChangeWeather),
    (321, Just RemoveEffectsResource),
    (322, Nothing),
    (323, Just TurnUndeadLevel),
    (324, Just ImmunityResourceMessage),
    (325, Just SavesAll),
    (326, Just ApplyEffectsList),
    (327, Just VisualSpellHit),
    (328, Just SetState),
    (329, Just SlowPoison),
    (330, Just FloatText),
    (331, Just SummonRandomMonster),
    (332, Just SpecificDamage),
    (333, Just StaticCharge),
    (334, Just TurnUndead),
    (335, Just SevenEyes),
    (336, Just DisplayEyes),
    (337, Just RemoveOpcode),
    (338, Just DisableRest),
    (339, Just AlterAnimation),
    (340, Just SetBackstabEffect),
    (341, Just SetCriticalHitEffect),
    (342, Just AnimationOverrideData),
    (343, Just HPSwap),
    (344, Just WeaponEnchantmentCreatureType),
    (345, Just WeaponEnchantmentBonus),
    (346, Just SavesSchoolBonus),
    (347, Nothing),
    (348, Nothing),
    (349, Nothing),
    (350, Nothing),
    (351, Nothing),
    (352, Nothing),
    (353, Nothing),
    (354, Nothing),
    (355, Nothing),
    (356, Nothing),
    (357, Nothing),
    (358, Nothing),
    (359, Nothing),
    (360, Just IgnoreReputationBreak),
    (361, Just SetCriticalMissEffect),
    (362, Just CriticalMiss),
    (363, Just ModalStateCheck),
    (364, Nothing),
    (365, Just MakeUnselectable),
    (366, Just EffectOnMove),
    (367, Just MinimumBaseStats)
    ]


{- | Vector @Index -> Maybe 'OpType'@. -}
opTypes :: Vector (Maybe OpType)
opTypes = force . fromList $ snd <$> optypes

{- | Vector of 'OpType' indices. -}
opIndices :: Vector Word32
opIndices = force . fromList . fmap fst . filter (\ (_ , r) -> isJust r) $ optypes

{- | Return the t'OpType' from an index. -}
toOpType :: Word32 -> Maybe OpType
toOpType m = if n <= length opTypes then opTypes ! n else Nothing
    where
        n = fromIntegral m

{- | Return the 'Word32' index associated to an t'OpType'. -}
fromOpType :: OpType -> Word32
fromOpType op = opIndices ! fromEnum op
