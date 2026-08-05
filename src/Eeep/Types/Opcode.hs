{- |
Module: Eeep.Types.Opcode

The @Opcode@ type.
-}

module Eeep.Types.Opcode (
    -- * Types.
    Opcode (..),

    -- ** Prisms.
    opcode,
) where

-- Imports.
-- Base.
import Data.Word (Word16, Word32)

-- Libraries.
import Optics.Core (Prism', prism', review, preview)

-- Package.
import Eeep.Types.Opcode.AC (AC, ac)
import Eeep.Types.Opcode.APR (APR, apr)


{- | The @OpType@ opcode type for all IE opcodes up to BG2 EE. -}
data Opcode
    = ACModifier !AC
    | APRModifier !APR
    | CureSleep
    -- | Berserk
    -- | CureBerserk
    -- | CharmSpecific
    -- | Charisma
    -- | ColorCharacterPalette
    -- | ColorRGB
    -- | ColorGlowPulse
    -- | Constitution
    -- | CurePoison
    -- | Damage
    -- | DeathInstant
    -- | Defrost
    -- | Dexterity
    -- | Haste
    -- | HPCurrent
    -- | HPMaximum
    -- | Intelligence
    -- | Invisibility
    -- | Lore
    -- | Luck
    -- | Morale
    -- | Horror
    -- | Poison
    -- | RemoveCursed
    -- | AcidResistance
    -- | ColdResitance
    -- | ElectricityResistance
    -- | FireResistance
    -- | MagicDamageResistance
    -- | CureDeath
    -- | SavesDeath
    -- | SavesWands
    -- | SavesPolymorph
    -- | SavesBreath
    -- | SavesSpells
    -- | Silence
    -- | Unconscious
    -- | Slow
    -- | Sparkle
    -- | WizardSpellSlots
    -- | CurePetrification
    -- | Strength
    -- | Stun
    -- | CureStun
    -- | CureInvisibilityAlternative
    -- | CureSilence
    -- | Wisdom
    -- | ColorGlowRGB
    -- | ColorDarkRGB
    -- | ColorBrightRGB
    -- | AnimationChange
    -- | THAC0
    -- | DeathCreatureType
    -- | AlignmentInvert
    -- | AlignmentChange
    -- | CureDispellable
    -- | Stealth
    -- | MiscastMagic
    -- | ColorFadeRGB
    -- | PriestSpellSlots
    -- | Infravision
    -- | RemoveInfravision
    -- | Blur
    -- | TransparencyFade
    -- | SummonCreature
    -- | UnsummonCreature
    -- | ImmunityDetection
    -- | CureImmunityDetection
    -- | ChangeSex
    -- | SetIDSState
    -- | ExtraDamage
    -- | Blindness
    -- | CureBlindness
    -- | Feebleminded
    -- | CureFeebleminded
    -- | Disease
    -- | CureDisease
    -- | Deafness
    -- | CureDeafness
    -- | SetAIScript
    -- | ImmunityProjectile
    -- | MagicFireResistance
    -- | MagicColdResistance
    -- | SlashingResistance
    -- | CrushingResistance
    -- | PiercinggResistance
    -- | MissileResistance
    -- | OpenLocks
    -- | FindTraps
    -- | PickPockets
    -- | Fatigue
    -- | Drunkenness
    -- | Tracking
    -- | Level
    -- | ExceptionalStrength
    -- | HPRegeneration
    -- | SpellEffectDuration
    -- | ImmunityCreatureType
    -- | ImmunityOpcode
    -- | SpellImmunityPower
    -- | ChangeName
    -- | Experience
    -- | Gold
    -- | MoraleBreak
    -- | ChangePortrait
    -- | Reputation
    -- | Paralyze
    -- | CreateMagicWeapon
    -- | RemoveItem
    -- | Dither
    -- | DetectAlignment
    -- | CureInvisibility
    -- | RevealArea
    -- | MirrorImage
    -- | ProtectionFromWeapons
    -- | CreateInventoryItem
    -- | RemoveInventoryItem
    -- | Teleport
    -- | Unlock
    -- | Movement
    -- | SummonMonster
    -- | Confusion
    -- | Aid
    -- | Bless
    -- | PositiveChant
    -- | RaisePhysicalStats
    -- | LuckNonCumulative
    -- | Petrification
    -- | PolymorphSpecific
    -- | ForceVisible
    -- | NegativeChant
    -- | CharacterAnimationChange
    -- | DisplayString
    -- | CastingGlow
    -- | LightingEffect
    -- | DisplayIcon
    -- | CreateItemSlot
    -- | DisableButton
    -- | DisableCasting
    -- | CastSpellCreature
    -- | LearnSpell
    -- | CastSpellPoint
    -- | EffectFindTraps
    -- | ReplaceCreature
    -- | PlayMovie
    -- | Sanctuary
    -- | Entangle
    -- | Globe
    -- | ProtectionMissiles
    -- | Web
    -- | Grease
    -- | EffectMirrorImage
    -- | RemoveSanctuary
    -- | CureHorror
    -- | CureParalyze
    -- | FreeAction
    -- | CureDrunkenness
    -- | EffectPauseTarget
    -- | MagicResistance
    -- | THAC0Missile
    -- | RemoveCreature
    -- | ImmunityIcon
    -- | DamageAnimation
    -- | LearnAbility
    -- | RemoveSpell
    -- | PoisonResistance
    -- | PlaySoundEffect
    -- | Hold
    -- | MovementUnconditioned
    -- | UseEff
    -- | THAC0CreatureType
    -- | DamageCreatureType
    -- | CannotUseItem
    -- | CannotUseItemType
    -- | ItemApplyEffect
    -- | ItemTypeApplyEffect
    -- | Passwall
    -- | HoldHelpless
    -- | MoveToArea
    -- | StoreLocalVariable
    -- | AuraCleanse
    -- | CastingTime
    -- | AttackSpeed
    -- | CastingLevel
    -- | FindFamiliar
    -- | TargetInvisible
    -- | IgnoreDialogPause
    -- | FamiliarBond
    -- | FamiliarBlock
    -- | BounceSpellProjectile
    -- | BounceSpellOpcode
    -- | BounceSpellPower
    -- | BounceSpellPowerDecrement
    -- | ImmunityPowerDecrement
    -- | BounceSpellSchool
    -- | BounceSpellSectype
    -- | ImmunitySpellSchool
    -- | ImmunitySpellSectype
    -- | ImmunitySpell
    -- | SpellBounceResource
    -- | HPMinimumLimit
    -- | DeathKill60
    -- | SpellStun90
    -- | Imprisonment
    -- | Freedom
    -- | Maze
    -- | SelectSpell
    -- | Play3DEffect
    -- | LevelDrain
    -- | Unconscious20
    -- | Stoneskin
    -- | ACCreatureType
    -- | RemoveSchool
    -- | RemoveSectype
    -- | TeleportField
    -- | ImmunitySchoolDecrement
    -- | CureLevelDrain
    -- | RevealMagic
    -- | ImmunitySectypeDecrement
    -- | BounceSpellSchoolDecrement
    -- | BounceSpellSectypeDecrement
    -- | RemoveSchoolOne
    -- | RemoveSectypeOne
    -- | TimeStop
    -- | CastSpellCondition
    -- | WeaponProficiency
    -- | CreateContingency
    -- | WingBuffet
    -- | ImageProjection
    -- | SpellPuppetID
    -- | DeathDisintegrate
    -- | Farsight
    -- | RemoveIcon
    -- | ControlCreature
    -- | CureConfusion
    -- | DrainCharges
    -- | DrainWizardSpell
    -- | CheckedBerserk
    -- | EffectBerserk
    -- | AttackNearest
    -- | SetMeleeEffect
    -- | SetRangedEffect
    -- | LuckDamage
    -- | ChangeBardSong
    -- | SetTrap
    -- | AddMapMarker
    -- | RemoveMapMarker
    -- | CreateInventoryItemWithDays
    -- | CreateSpellSequencer
    -- | CreateCustomSpellSequencer
    -- | ActivateSpellSequencerCreature
    -- | TrapPowerDecrement
    -- | ActivateSpellSequencerPoint
    -- | RestoreSpells
    -- | VisualRange
    -- | Backstab
    -- | DropWeapons
    -- | ModifyGlobalVariable
    -- | RemoveImmunitySpell
    -- | ImmunityDisplayString
    -- | ClearFog
    -- | ShakeWindow
    -- | CurePauseTarget
    -- | RemoveAvatar
    -- | RepeatingEff
    -- | RemoveAreaProjectile
    -- | TeleportToTarget
    -- | HideInShadows
    -- | DetectIllusion
    -- | SetTraps
    -- | ModifiedTHAC0
    -- | EnableButton
    -- | EffectWildMagic
    -- | WildMagic
    -- | ScriptState
    -- | UseEffCursed
    -- | MeleeTHAC0
    -- | MeleeDamage
    -- | RangedDamage
    -- | RemoveSelectionCircle
    -- | FistTHAC0
    -- | FistDamage
    -- | ChangeTitle
    -- | ImmunityVisualEffect
    -- | ImmunityBackstab
    -- | EnableOffScreenAI
    -- | SpellExistenceDelay
    -- | DisablePermanentDeath
    -- | ImmunitySpecificAnimation
    -- | ImmunityTurnUndead
    -- | TeleportPocketPlane
    -- | ChaosShield
    -- | NPCBump
    -- | CriticalHit
    -- | CanUseAnyItem
    -- | Assassinate
    -- | MassRaiseDead
    -- | THAC0Offhand
    -- | THAC0Onhand
    -- | AbilityTracking
    -- | ImmunityTracking
    -- | ModifyLocalVariable
    -- | ImmunityTimeStop
    -- | RandomWish
    -- | ImmunitySequester
    -- | GainedHLA
    -- | GolemStoneskin
    -- | AnimationRemoval
    -- | MagicRest
    -- | HasteUnconditioned
    -- | ImmunityResource
    -- | ItemUsability
    -- | ChangeWeather
    -- | RemoveEffectsResource
    -- | TurnUndeadLevel
    -- | ImmunityResourceMessage
    -- | SavesAll
    -- | ApplyEffectsList
    -- | VisualSpellHit
    -- | SetState
    -- | SlowPoison
    -- | FloatText
    -- | SummonRandomMonster
    -- | SpecificDamage
    -- | StaticCharge
    -- | TurnUndead
    -- | SevenEyes
    -- | DisplayEyes
    -- | RemoveOpcode
    -- | DisableRest
    -- | AlterAnimation
    -- | SetBackstabEffect
    -- | SetCriticalHitEffect
    -- | AnimationOverrideData
    -- | HPSwap
    -- | WeaponEnchantmentCreatureType
    -- | WeaponEnchantmentBonus
    -- | SavesSchoolBonus
    -- | IgnoreReputationBreak
    -- | SetCriticalMissEffect
    -- | CriticalMiss
    -- | ModalStateCheck
    -- | MakeUnselectable
    -- | EffectOnMove
    -- | MinimumBaseStats
    deriving stock (Eq, Ord, Show)


{- | The prism for the t'Opcode' type. -}
{-# INLINABLE opcode #-}
opcode :: Prism' (Word16, Word32, Word32) Opcode
opcode = prism' construct match
    where
        construct :: Opcode -> (Word16, Word32, Word32)
        construct (ACModifier a)  = let (t, m) = review ac a in (0, t, m)
        construct (APRModifier a) = let (t, m) = review apr a in (1, t, m)
        construct CureSleep       = (2, 0, 0)

        match :: (Word16, Word32, Word32) -> Maybe Opcode
        match (0, p, q) = fmap ACModifier (preview ac (p, q))
        match (1, p, q) = fmap APRModifier (preview apr (p, q))
        match (2, _, _) = Just CureSleep
        match (_, _, _) = Nothing
