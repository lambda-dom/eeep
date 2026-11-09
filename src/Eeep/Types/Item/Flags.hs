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
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', Lens', (%), coercedTo, view)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Data.Functor.Contravariant (Contravariant(..))


{- | The spell @Flags@ type.

note(s):

    * The 'Ord' and 'Bounded' instances have no significance and are present merely to be able to
    use t'Flags' values as keys in maps, etc.
-}
newtype Flags = Flags Word32
    deriving stock (Eq, Show)


{- | Smart constructor for the t'Flags' type.

note(s)

    * The constructor performs normalization so it is *not* an inverse of 'fromFlags', only a
    right-inverse. By the same token, it is not part of a 'Optics.Core.Prism'.
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
fromFlags = view flags


-- | Prism for the t'Flags' type. Because of normalization, not exported.
{-# INLINE flags #-}
flags :: Iso' Flags Word32
flags = coercedTo


{- | The unsellable t'Flags' bit lens. -}
{-# INLINE unsellable #-}
unsellable :: Lens' Flags Bool
unsellable = flags % bitAt 0

{- | The twohanded t'Flags' bit lens. -}
{-# INLINE twoHanded #-}
twoHanded :: Lens' Flags Bool
twoHanded = flags % bitAt 1

{- | The droppable t'Flags' bit lens. -}
{-# INLINE droppable #-}
droppable :: Lens' Flags Bool
droppable = flags % bitAt 2

{- | The displayable t'Flags' bit lens. -}
{-# INLINE displayable #-}
displayable :: Lens' Flags Bool
displayable = flags % bitAt 3

{- | The cursed t'Flags' bit lens. -}
{-# INLINE cursed #-}
cursed :: Lens' Flags Bool
cursed = flags % bitAt 4

{- | The cloneable t'Flags' bit lens. -}
{-# INLINE cloneable #-}
cloneable :: Lens' Flags Bool
cloneable = flags % bitAt 5

{- | The magical t'Flags' bit lens. -}
{-# INLINE magical #-}
magical :: Lens' Flags Bool
magical = flags % bitAt 6

{- | The leftHanded t'Flags' bit lens. -}
{-# INLINE leftHanded #-}
leftHanded :: Lens' Flags Bool
leftHanded = flags % bitAt 7

{- | The silver t'Flags' bit lens. -}
{-# INLINE silver #-}
silver :: Lens' Flags Bool
silver = flags % bitAt 8

{- | The coldIron t'Flags' bit lens. -}
{-# INLINE coldIron #-}
coldIron :: Lens' Flags Bool
coldIron = flags % bitAt 9

{- | The offHanded t'Flags' bit lens. -}
{-# INLINE offHanded #-}
offHanded :: Lens' Flags Bool
offHanded = flags % bitAt 10

{- | The conversable t'Flags' bit lens. -}
{-# INLINE conversable #-}
conversable :: Lens' Flags Bool
conversable = flags % bitAt 11

{- | The twohandedFake t'Flags' bit lens. -}
{-# INLINE twoHandedFake #-}
twoHandedFake :: Lens' Flags Bool
twoHandedFake = flags % bitAt 12

{- | The noOffHanded t'Flags' bit lens. -}
{-# INLINE noOffHanded #-}
noOffHanded :: Lens' Flags Bool
noOffHanded = flags % bitAt 13

{- | The adamantine t'Flags' bit lens. -}
{-# INLINE adamantine #-}
adamantine :: Lens' Flags Bool
adamantine = flags % bitAt 15

{- | The undispellable t'Flags' bit lens. -}
{-# INLINE undispellable #-}
undispellable :: Lens' Flags Bool
undispellable = flags % bitAt 24

{- | The criticalAversion t'Flags' bit lens. -}
{-# INLINE criticalAversion #-}
criticalAversion :: Lens' Flags Bool
criticalAversion = flags % bitAt 25


{- | Decode a t'Flags'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Flags
decode = fmap toFlags Parsers.word32Le

{- | Encode a t'Flags'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Flags
encode = contramap fromFlags Serializers.word32Le
