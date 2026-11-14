{- |
Module: Eeep.Types.Item.KitUsability

The @KitUsability@ type for the per kit usability flags field of the main item header.

note(s):

    * No custom parser and serializer provided, because the four exclusion bytes are not laid out
    consecutively.
-}

module Eeep.Types.Item.KitUsability (
    -- * Types.
    KitUsability,

    -- ** Isomorphisms.
    exclusionBytes,

    -- ** Traversals.
    kitUsability,

    -- ** Bit lenses.
    talos,
    helm,
    lathander,
    totemic,
    shapeshifter,
    avenger,
    barbarian,
    wildmage,
    stalker,
    beastmaster,
    assassin,
    bountyHunter,
    swashbuckler,
    blade,
    jester,
    skald,
    diviner,
    enchanter,
    illusionist,
    invoker,
    necromancer,
    transmuter,
    kitless,
    archer,
    berserker,
    wizardSlayer,
    kensai,
    cavalier,
    inquisitor,
    undeadHunter,
    abjurer,
    conjurer,
) where

-- Imports.
-- Base.
import Data.Word (Word32, Word8)

-- Libraries.
import Optics.Core (Iso', Lens', IxTraversal', (%), coercedTo)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt, word32BytesLe, bits)


{- | The @KitUsability@ type for the per race/class usability flags field of the main item header.

No parser or serializer is provided due to out the t'KitUsability' data is layed out.

note(s):

    * The 'Ord' and 'Bounded' instances have no significance and are present merely to be able to
    use t'KitUsability' values as keys in maps, etc.
-}
newtype KitUsability = KitUsability Word32
    deriving stock (Eq, Ord, Bounded, Show)


-- | Isomorphism for the t'KitUsability' type. Not exported.
{-# INLINE kitUsabilityFlags #-}
kitUsabilityFlags :: Iso' KitUsability Word32
kitUsabilityFlags = coercedTo

{- | Isomorphism between the t'KitUsability' type and the 4 exclusion bytes. -}
{-# INLINE exclusionBytes #-}
exclusionBytes :: Iso' KitUsability (Word8, Word8, Word8, Word8)
exclusionBytes = kitUsabilityFlags % word32BytesLe


{- | Indexed traversal over the kit usability bits. -}
{-# INLINE kitUsability #-}
kitUsability :: IxTraversal' Int KitUsability Bool
kitUsability = kitUsabilityFlags % bits


{- | The Talos Cleric t'KitUsability' bit lens. -}
{-# INLINE talos #-}
talos :: Lens' KitUsability Bool
talos = kitUsabilityFlags % bitAt 0

{- | The Helm Cleric t'KitUsability' bit lens. -}
{-# INLINE helm #-}
helm :: Lens' KitUsability Bool
helm = kitUsabilityFlags % bitAt 1

{- | The Lathander Cleric t'KitUsability' bit lens. -}
{-# INLINE lathander #-}
lathander :: Lens' KitUsability Bool
lathander = kitUsabilityFlags % bitAt 2

{- | The Totemic Druid t'KitUsability' bit lens. -}
{-# INLINE totemic #-}
totemic :: Lens' KitUsability Bool
totemic = kitUsabilityFlags % bitAt 3

{- | The Shapeshifter Druid t'KitUsability' bit lens. -}
{-# INLINE shapeshifter #-}
shapeshifter :: Lens' KitUsability Bool
shapeshifter = kitUsabilityFlags % bitAt 4

{- | The Avenger Druid t'KitUsability' bit lens. -}
{-# INLINE avenger #-}
avenger :: Lens' KitUsability Bool
avenger = kitUsabilityFlags % bitAt 5

{- | The barbarian t'KitUsability' bit lens. -}
{-# INLINE barbarian #-}
barbarian :: Lens' KitUsability Bool
barbarian = kitUsabilityFlags % bitAt 6

{- | The wildmage t'KitUsability' bit lens. -}
{-# INLINE wildmage #-}
wildmage :: Lens' KitUsability Bool
wildmage = kitUsabilityFlags % bitAt 7

{- | The Stalker Ranger t'KitUsability' bit lens. -}
{-# INLINE stalker #-}
stalker :: Lens' KitUsability Bool
stalker = kitUsabilityFlags % bitAt 8

{- | The Beastmaster Ranger t'KitUsability' bit lens. -}
{-# INLINE beastmaster #-}
beastmaster :: Lens' KitUsability Bool
beastmaster = kitUsabilityFlags % bitAt 9

{- | The Assassin t'KitUsability' bit lens. -}
{-# INLINE assassin #-}
assassin :: Lens' KitUsability Bool
assassin = kitUsabilityFlags % bitAt 10

{- | The Bounty-Hunter t'KitUsability' bit lens. -}
{-# INLINE bountyHunter #-}
bountyHunter :: Lens' KitUsability Bool
bountyHunter = kitUsabilityFlags % bitAt 11

{- | The Swashbuckler t'KitUsability' bit lens. -}
{-# INLINE swashbuckler #-}
swashbuckler :: Lens' KitUsability Bool
swashbuckler = kitUsabilityFlags % bitAt 12

{- | The Blade t'KitUsability' bit lens. -}
{-# INLINE blade #-}
blade :: Lens' KitUsability Bool
blade = kitUsabilityFlags % bitAt 13

{- | The Jester t'KitUsability' bit lens. -}
{-# INLINE jester #-}
jester :: Lens' KitUsability Bool
jester = kitUsabilityFlags % bitAt 14

{- | The Skald t'KitUsability' bit lens. -}
{-# INLINE skald #-}
skald :: Lens' KitUsability Bool
skald = kitUsabilityFlags % bitAt 15

{- | The Diviner t'KitUsability' bit lens. -}
{-# INLINE diviner #-}
diviner :: Lens' KitUsability Bool
diviner = kitUsabilityFlags % bitAt 16

{- | The Enchanter t'KitUsability' bit lens. -}
{-# INLINE enchanter #-}
enchanter :: Lens' KitUsability Bool
enchanter = kitUsabilityFlags % bitAt 17

{- | The Illusionist t'KitUsability' bit lens. -}
{-# INLINE illusionist #-}
illusionist :: Lens' KitUsability Bool
illusionist = kitUsabilityFlags % bitAt 18

{- | The Invoker t'KitUsability' bit lens. -}
{-# INLINE invoker #-}
invoker :: Lens' KitUsability Bool
invoker = kitUsabilityFlags % bitAt 19

{- | The Necromancer t'KitUsability' bit lens. -}
{-# INLINE necromancer #-}
necromancer :: Lens' KitUsability Bool
necromancer = kitUsabilityFlags % bitAt 20

{- | The transmuter t'KitUsability' bit lens. -}
{-# INLINE transmuter #-}
transmuter :: Lens' KitUsability Bool
transmuter = kitUsabilityFlags % bitAt 21

{- | The kitless t'KitUsability' bit lens. -}
{-# INLINE kitless #-}
kitless :: Lens' KitUsability Bool
kitless = kitUsabilityFlags % bitAt 22

{- | The Archer t'KitUsability' bit lens. -}
{-# INLINE archer #-}
archer :: Lens' KitUsability Bool
archer = kitUsabilityFlags % bitAt 23

{- | The Berserker t'KitUsability' bit lens. -}
{-# INLINE berserker #-}
berserker :: Lens' KitUsability Bool
berserker = kitUsabilityFlags % bitAt 24

{- | The Wizard Slayer t'KitUsability' bit lens. -}
{-# INLINE wizardSlayer #-}
wizardSlayer :: Lens' KitUsability Bool
wizardSlayer = kitUsabilityFlags % bitAt 25

{- | The Kensai t'KitUsability' bit lens. -}
{-# INLINE kensai #-}
kensai :: Lens' KitUsability Bool
kensai = kitUsabilityFlags % bitAt 26

{- | The Cavalier t'KitUsability' bit lens. -}
{-# INLINE cavalier #-}
cavalier :: Lens' KitUsability Bool
cavalier = kitUsabilityFlags % bitAt 27

{- | The Inquisitor t'KitUsability' bit lens. -}
{-# INLINE inquisitor #-}
inquisitor :: Lens' KitUsability Bool
inquisitor = kitUsabilityFlags % bitAt 28

{- | The Undead Hunter t'KitUsability' bit lens. -}
{-# INLINE undeadHunter #-}
undeadHunter :: Lens' KitUsability Bool
undeadHunter = kitUsabilityFlags % bitAt 29

{- | The Abjurer t'KitUsability' bit lens. -}
{-# INLINE abjurer #-}
abjurer :: Lens' KitUsability Bool
abjurer = kitUsabilityFlags % bitAt 30

{- | The Conjurer t'KitUsability' bit lens. -}
{-# INLINE conjurer #-}
conjurer :: Lens' KitUsability Bool
conjurer = kitUsabilityFlags % bitAt 31
