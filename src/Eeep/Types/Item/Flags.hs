{- |
Module: Eeep.Types.Item.Flags

The @Flags@ type for the flags field of the main item header.
-}

module Eeep.Types.Item.Flags (
    -- * Types.
    Flags,

    -- ** Constructors and eliminators.
    toFlags,
    fromFlags,

    -- ** Traversals.
    flags,

    -- ** Bit lenses.
    unsellable,
    twoHanded,
    droppable,
    displayable,
    cursed,
    cloneable,
    magical,
    leftHanded,
    silver,
    coldIron,
    offHanded,
    conversable,
    twoHandedFake,
    noOffHanded,
    adamantine,
    undispellable,
    criticalAversion,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Bits ((.&.))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', Lens', IxTraversal', (%), coercedTo, view, elementsOf, toListOf)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt, bits)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The item @Flags@ type.

note(s):

    * The 'Ord' and 'Bounded' instances have no significance and are present merely to be able to
    use t'Flags' values as keys in maps, etc.
-}
newtype Flags = Flags Word32
    deriving stock (Eq, Ord, Bounded)


-- Instances.
instance Show Flags where
    show :: Flags -> String
    show = show . toListOf flags


{- | Smart constructor for the t'Flags' type.

note(s)

    * The constructor performs normalization so it is *not* an inverse of 'fromFlags', only a
    right-inverse. For the same reason, it is not part of a 'Optics.Core.Prism'.
-}
{-# INLINE toFlags #-}
toFlags :: Word32 -> Flags
toFlags = Flags . (.&. mask)
    where
        mask :: Word32
        mask = 50380799

{- | Eliminator for the t'Flags' type. -}
{-# INLINE fromFlags #-}
fromFlags :: Flags -> Word32
fromFlags = view itemFlags


-- | Isomorphism for the t'Flags' type. Because of normalization, not exported.
{-# INLINE itemFlags #-}
itemFlags :: Iso' Flags Word32
itemFlags = coercedTo


{- | Indexed traversal over the valid bit flags. -}
{-# INLINE flags #-}
flags :: IxTraversal' Int Flags Bool
flags = elementsOf (itemFlags % bits) p
    where
        p :: Int -> Bool
        p n = (0 <= n && n <= 13) || (24 <= n && n <= 25) || (n == 15)


{- | The unsellable t'Flags' bit lens. -}
{-# INLINE unsellable #-}
unsellable :: Lens' Flags Bool
unsellable = itemFlags % bitAt 0

{- | The twohanded t'Flags' bit lens. -}
{-# INLINE twoHanded #-}
twoHanded :: Lens' Flags Bool
twoHanded = itemFlags % bitAt 1

{- | The droppable t'Flags' bit lens. -}
{-# INLINE droppable #-}
droppable :: Lens' Flags Bool
droppable = itemFlags % bitAt 2

{- | The displayable t'Flags' bit lens. -}
{-# INLINE displayable #-}
displayable :: Lens' Flags Bool
displayable = itemFlags % bitAt 3

{- | The cursed t'Flags' bit lens. -}
{-# INLINE cursed #-}
cursed :: Lens' Flags Bool
cursed = itemFlags % bitAt 4

{- | The cloneable t'Flags' bit lens. -}
{-# INLINE cloneable #-}
cloneable :: Lens' Flags Bool
cloneable = itemFlags % bitAt 5

{- | The magical t'Flags' bit lens. -}
{-# INLINE magical #-}
magical :: Lens' Flags Bool
magical = itemFlags % bitAt 6

{- | The leftHanded t'Flags' bit lens. -}
{-# INLINE leftHanded #-}
leftHanded :: Lens' Flags Bool
leftHanded = itemFlags % bitAt 7

{- | The silver t'Flags' bit lens. -}
{-# INLINE silver #-}
silver :: Lens' Flags Bool
silver = itemFlags % bitAt 8

{- | The coldIron t'Flags' bit lens. -}
{-# INLINE coldIron #-}
coldIron :: Lens' Flags Bool
coldIron = itemFlags % bitAt 9

{- | The offHanded t'Flags' bit lens. -}
{-# INLINE offHanded #-}
offHanded :: Lens' Flags Bool
offHanded = itemFlags % bitAt 10

{- | The conversable t'Flags' bit lens. -}
{-# INLINE conversable #-}
conversable :: Lens' Flags Bool
conversable = itemFlags % bitAt 11

{- | The twohandedFake t'Flags' bit lens. -}
{-# INLINE twoHandedFake #-}
twoHandedFake :: Lens' Flags Bool
twoHandedFake = itemFlags % bitAt 12

{- | The noOffHanded t'Flags' bit lens. -}
{-# INLINE noOffHanded #-}
noOffHanded :: Lens' Flags Bool
noOffHanded = itemFlags % bitAt 13

{- | The adamantine t'Flags' bit lens. -}
{-# INLINE adamantine #-}
adamantine :: Lens' Flags Bool
adamantine = itemFlags % bitAt 15

{- | The undispellable t'Flags' bit lens. -}
{-# INLINE undispellable #-}
undispellable :: Lens' Flags Bool
undispellable = itemFlags % bitAt 24

{- | The criticalAversion t'Flags' bit lens. -}
{-# INLINE criticalAversion #-}
criticalAversion :: Lens' Flags Bool
criticalAversion = itemFlags % bitAt 25


{- | Decode a t'Flags'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary s => Parser s InputError Flags
decode = fmap toFlags Parsers.word32Le

{- | Encode a t'Flags'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Flags
encode = contramap fromFlags Serializers.word32Le
