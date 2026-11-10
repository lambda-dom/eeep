{- |
Module: Eeep.Types.Item.Ability.Flags

The @Flags@ type for the flags field of an item ability.
-}

module Eeep.Types.Item.Ability.Flags (
    -- * Types.
    Flags,

    -- ** Constructors and eliminators.
    toFlags,
    fromFlags,

    -- ** Bit lenses.
    strengthBonus,

    -- ** Parsers and serializers.
    decode,
    encode,
    breakable,
    damageBonus,
    thac0Bonus,
    breakInvis,
    hostile,
    rechargeRest,
    noInvis,
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


{- | The item @Flags@ type.

note(s):

    * The 'Ord' and 'Bounded' instances have no significance and are present merely to be able to
    use t'Flags' values as keys in maps, etc.
-}
newtype Flags = Flags Word32
    deriving stock (Eq, Ord, Bounded, Show)


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
        mask = 67112463

{- | Eliminator for the t'Flags' type. -}
{-# INLINE fromFlags #-}
fromFlags :: Flags -> Word32
fromFlags = view flags


-- | Isomorphism for the t'Flags' type. Because of normalization, not exported.
{-# INLINE flags #-}
flags :: Iso' Flags Word32
flags = coercedTo


{- | The strengthBonus t'Flags' bit lens. -}
{-# INLINE strengthBonus #-}
strengthBonus :: Lens' Flags Bool
strengthBonus = flags % bitAt 0

{- | The breakable t'Flags' bit lens. -}
{-# INLINE breakable #-}
breakable :: Lens' Flags Bool
breakable = flags % bitAt 1

{- | The damageBonus t'Flags' bit lens. -}
{-# INLINE damageBonus #-}
damageBonus :: Lens' Flags Bool
damageBonus = flags % bitAt 2

{- | The thac0Bonus t'Flags' bit lens. -}
{-# INLINE thac0Bonus #-}
thac0Bonus :: Lens' Flags Bool
thac0Bonus = flags % bitAt 3

{- | The breakInvis t'Flags' bit lens. -}
{-# INLINE breakInvis #-}
breakInvis :: Lens' Flags Bool
breakInvis = flags % bitAt 9

{- | The hostile t'Flags' bit lens. -}
{-# INLINE hostile #-}
hostile :: Lens' Flags Bool
hostile = flags % bitAt 10

{- | The rechargeRest t'Flags' bit lens. -}
{-# INLINE rechargeRest #-}
rechargeRest :: Lens' Flags Bool
rechargeRest = flags % bitAt 11

{- | The noInvis t'Flags' bit lens. -}
{-# INLINE noInvis #-}
noInvis :: Lens' Flags Bool
noInvis = flags % bitAt 26


{- | Decode a t'Flags'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Flags
decode = fmap toFlags Parsers.word32Le

{- | Encode a t'Flags'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Flags
encode = contramap fromFlags Serializers.word32Le
