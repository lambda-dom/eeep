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

    -- ** Traversals.
    flags,

    -- ** Bit lenses.
    strengthBonus,
    breakable,
    damageBonus,
    thac0Bonus,
    breakInvis,
    hostile,
    rechargeRest,
    noInvis,

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
    right-inverse. By the same reason, it is not part of a 'Optics.Core.Prism'.
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
fromFlags = view abilityFlags


-- | Isomorphism for the t'Flags' type. Because of normalization, not exported.
{-# INLINE abilityFlags #-}
abilityFlags :: Iso' Flags Word32
abilityFlags = coercedTo


{- | Indexed traversal over the valid bit flags. -}
{-# INLINE flags #-}
flags :: IxTraversal' Int Flags Bool
flags = elementsOf (abilityFlags % bits) p
    where
        p :: Int -> Bool
        p n = (0 <= n && n <= 3) || (9 <= n && n <= 11) || (n == 26)


{- | The strengthBonus t'Flags' bit lens. -}
{-# INLINE strengthBonus #-}
strengthBonus :: Lens' Flags Bool
strengthBonus = abilityFlags % bitAt 0

{- | The breakable t'Flags' bit lens. -}
{-# INLINE breakable #-}
breakable :: Lens' Flags Bool
breakable = abilityFlags % bitAt 1

{- | The damageBonus t'Flags' bit lens. -}
{-# INLINE damageBonus #-}
damageBonus :: Lens' Flags Bool
damageBonus = abilityFlags % bitAt 2

{- | The thac0Bonus t'Flags' bit lens. -}
{-# INLINE thac0Bonus #-}
thac0Bonus :: Lens' Flags Bool
thac0Bonus = abilityFlags % bitAt 3

{- | The breakInvis t'Flags' bit lens. -}
{-# INLINE breakInvis #-}
breakInvis :: Lens' Flags Bool
breakInvis = abilityFlags % bitAt 9

{- | The hostile t'Flags' bit lens. -}
{-# INLINE hostile #-}
hostile :: Lens' Flags Bool
hostile = abilityFlags % bitAt 10

{- | The rechargeRest t'Flags' bit lens. -}
{-# INLINE rechargeRest #-}
rechargeRest :: Lens' Flags Bool
rechargeRest = abilityFlags % bitAt 11

{- | The noInvis t'Flags' bit lens. -}
{-# INLINE noInvis #-}
noInvis :: Lens' Flags Bool
noInvis = abilityFlags % bitAt 26


{- | Decode a t'Flags'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Flags
decode = fmap toFlags Parsers.word32Le

{- | Encode a t'Flags'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Flags
encode = contramap fromFlags Serializers.word32Le
