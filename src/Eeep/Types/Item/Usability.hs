{- |
Module: Eeep.Types.Item.Usability

The @Usability@ type for the per race/class usability flags field of the main item header.
-}

module Eeep.Types.Item.Usability (
    -- * Types.
    Usability,

    -- ** Traversals.
    usability,

    -- ** Bit lenses.
    chaotic,
    evil,
    good,
    lcneutral,
    lawful,
    geneutral,
    bard,
    cleric,
    clericMage,
    clericThief,
    clericRanger,
    fighter,
    fighterDruid,
    fighterMage,
    fighterCleric,
    fighterMageCleric,
    fighterMageThief,
    fighterThief,
    mage,
    mageThief,
    paladin,
    ranger,
    thief,
    elf,
    dwarf,
    halfElf,
    halfling,
    human,
    gnome,
    monk,
    druid,
    halfOrc,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', Lens', (%), coercedTo, view, review, IxTraversal')

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt, bits)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The @Usability@ type for the per race/class usability flags field of the main item header.

note(s):

    * The 'Ord' and 'Bounded' instances have no significance and are present merely to be able to
    use t'Usability' values as keys in maps, etc.
-}
newtype Usability = Usability Word32
    deriving stock (Eq, Ord, Bounded, Show)


{- | Isomorphism for the t'Usability' type. -}
{-# INLINE usabilityFlags #-}
usabilityFlags :: Iso' Usability Word32
usabilityFlags = coercedTo


{- | Indexed traversal over the usability bits. -}
{-# INLINE usability #-}
usability :: IxTraversal' Int Usability Bool
usability = usabilityFlags % bits


{- | The chaotic t'Usability' bit lens. -}
{-# INLINE chaotic #-}
chaotic :: Lens' Usability Bool
chaotic = usabilityFlags % bitAt 0

{- | The evil t'Usability' bit lens. -}
{-# INLINE evil #-}
evil :: Lens' Usability Bool
evil = usabilityFlags % bitAt 1

{- | The good t'Usability' bit lens. -}
{-# INLINE good #-}
good :: Lens' Usability Bool
good = usabilityFlags % bitAt 2

{- | The lawful-chaotic axis neutral t'Usability' bit lens. -}
{-# INLINE lcneutral #-}
lcneutral :: Lens' Usability Bool
lcneutral = usabilityFlags % bitAt 3

{- | The lawful t'Usability' bit lens. -}
{-# INLINE lawful #-}
lawful :: Lens' Usability Bool
lawful = usabilityFlags % bitAt 4

{- | The good-evil axis neutral t'Usability' bit lens. -}
{-# INLINE geneutral #-}
geneutral :: Lens' Usability Bool
geneutral = usabilityFlags % bitAt 5

{- | The bard t'Usability' bit lens. -}
{-# INLINE bard #-}
bard :: Lens' Usability Bool
bard = usabilityFlags % bitAt 6

{- | The cleric t'Usability' bit lens. -}
{-# INLINE cleric #-}
cleric :: Lens' Usability Bool
cleric = usabilityFlags % bitAt 7

{- | The cleric-mage t'Usability' bit lens. -}
{-# INLINE clericMage #-}
clericMage :: Lens' Usability Bool
clericMage = usabilityFlags % bitAt 8

{- | The cleric-thief t'Usability' bit lens. -}
{-# INLINE clericThief #-}
clericThief :: Lens' Usability Bool
clericThief = usabilityFlags % bitAt 9

{- | The cleric-ranger t'Usability' bit lens. -}
{-# INLINE clericRanger #-}
clericRanger :: Lens' Usability Bool
clericRanger = usabilityFlags % bitAt 10

{- | The fighter t'Usability' bit lens. -}
{-# INLINE fighter #-}
fighter :: Lens' Usability Bool
fighter = usabilityFlags % bitAt 11

{- | The fighter-druid t'Usability' bit lens. -}
{-# INLINE fighterDruid #-}
fighterDruid :: Lens' Usability Bool
fighterDruid = usabilityFlags % bitAt 12

{- | The fighter-mage t'Usability' bit lens. -}
{-# INLINE fighterMage #-}
fighterMage :: Lens' Usability Bool
fighterMage = usabilityFlags % bitAt 13

{- | The fighter-cleric t'Usability' bit lens. -}
{-# INLINE fighterCleric #-}
fighterCleric :: Lens' Usability Bool
fighterCleric = usabilityFlags % bitAt 14

{- | The fighter-mage-cleric t'Usability' bit lens. -}
{-# INLINE fighterMageCleric #-}
fighterMageCleric :: Lens' Usability Bool
fighterMageCleric = usabilityFlags % bitAt 15

{- | The fighter-mage-thief t'Usability' bit lens. -}
{-# INLINE fighterMageThief #-}
fighterMageThief :: Lens' Usability Bool
fighterMageThief = usabilityFlags % bitAt 16

{- | The fighter-thief t'Usability' bit lens. -}
{-# INLINE fighterThief #-}
fighterThief :: Lens' Usability Bool
fighterThief = usabilityFlags % bitAt 17

{- | The mage t'Usability' bit lens. -}
{-# INLINE mage #-}
mage :: Lens' Usability Bool
mage = usabilityFlags % bitAt 18

{- | The mage-thief t'Usability' bit lens. -}
{-# INLINE mageThief #-}
mageThief :: Lens' Usability Bool
mageThief = usabilityFlags % bitAt 19

{- | The paladin t'Usability' bit lens. -}
{-# INLINE paladin #-}
paladin :: Lens' Usability Bool
paladin = usabilityFlags % bitAt 20

{- | The ranger t'Usability' bit lens. -}
{-# INLINE ranger #-}
ranger :: Lens' Usability Bool
ranger = usabilityFlags % bitAt 21

{- | The thief t'Usability' bit lens. -}
{-# INLINE thief #-}
thief :: Lens' Usability Bool
thief = usabilityFlags % bitAt 22

{- | The elf t'Usability' bit lens. -}
{-# INLINE elf #-}
elf :: Lens' Usability Bool
elf = usabilityFlags % bitAt 23

{- | The dwarf t'Usability' bit lens. -}
{-# INLINE dwarf #-}
dwarf :: Lens' Usability Bool
dwarf = usabilityFlags % bitAt 24

{- | The half-elf t'Usability' bit lens. -}
{-# INLINE halfElf #-}
halfElf :: Lens' Usability Bool
halfElf = usabilityFlags % bitAt 25

{- | The halfling t'Usability' bit lens. -}
{-# INLINE halfling #-}
halfling :: Lens' Usability Bool
halfling = usabilityFlags % bitAt 26

{- | The human t'Usability' bit lens. -}
{-# INLINE human #-}
human :: Lens' Usability Bool
human = usabilityFlags % bitAt 27

{- | The gnome t'Usability' bit lens. -}
{-# INLINE gnome #-}
gnome :: Lens' Usability Bool
gnome = usabilityFlags % bitAt 28

{- | The monk t'Usability' bit lens. -}
{-# INLINE monk #-}
monk :: Lens' Usability Bool
monk = usabilityFlags % bitAt 29

{- | The druid t'Usability' bit lens. -}
{-# INLINE druid #-}
druid :: Lens' Usability Bool
druid = usabilityFlags % bitAt 30

{- | The half-orc t'Usability' bit lens. -}
{-# INLINE halfOrc #-}
halfOrc :: Lens' Usability Bool
halfOrc = usabilityFlags % bitAt 31


{- | Decode a t'Usability'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary s => Parser s InputError Usability
decode = fmap (review usabilityFlags) Parsers.word32Le

{- | Encode a t'Usability'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Usability
encode = contramap (view usabilityFlags) Serializers.word32Le
