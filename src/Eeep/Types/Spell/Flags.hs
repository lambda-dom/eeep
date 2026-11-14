{- |
Module: Eeep.Types.Spell.Flags

The @Flags@ type for the flags field of the main spell header.
-}

module Eeep.Types.Spell.Flags (
    -- * Types.
    Flags,

    -- ** Constructors and eliminators.
    toFlags,
    fromFlags,

    -- ** Traversals.
    flags,

    -- ** Bit lenses.
    breakInvis,
    hostile,
    noLos,
    allowSpot,
    outdoors,
    ignoreMagic,
    ignoreSurge,
    notCombat,
    targetInvis,
    castSilenced,

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
import Optics.Core (Iso', Lens', (%), coercedTo, view, IxTraversal', elementsOf, toListOf)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt, bits)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The spell @Flags@ type.

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


{- | Smart constructor for the spell t'Flags' type.

note(s)

    * The constructor performs normalization so it is *not* an inverse of 'fromFlags', only a
    right-inverse. By the same reason, it is not part of a 'Optics.Core.Prism'.
-}
{-# INLINE toFlags #-}
toFlags :: Word32 -> Flags
toFlags = Flags . (.&. mask)
    where
        mask :: Word32
        mask = 50462208

{- | Eliminator for the t'Flags' type. -}
{-# INLINE fromFlags #-}
fromFlags :: Flags -> Word32
fromFlags = view spellFlags


-- | Isomorphism for the t'Flags' type. Not exported.
{-# INLINE spellFlags #-}
spellFlags :: Iso' Flags Word32
spellFlags = coercedTo


{- | Indexed traversal over the valid bit flags. -}
{-# INLINE flags #-}
flags :: IxTraversal' Int Flags Bool
flags = elementsOf (spellFlags % bits) p
    where
        p :: Int -> Bool
        p n = (9 <= n && n <= 16) || (24 <= n && n <= 25)


{- | The breakInvis t'Flags' bit lens. -}
{-# INLINE breakInvis #-}
breakInvis :: Lens' Flags Bool
breakInvis = spellFlags % bitAt 9

{- | The hostile t'Flags' bit lens. -}
{-# INLINE hostile #-}
hostile :: Lens' Flags Bool
hostile = spellFlags % bitAt 10

{- | The noLos t'Flags' bit lens. -}
{-# INLINE noLos #-}
noLos :: Lens' Flags Bool
noLos = spellFlags % bitAt 11

{- | The allowSpot t'Flags' bit lens. -}
{-# INLINE allowSpot #-}
allowSpot :: Lens' Flags Bool
allowSpot = spellFlags % bitAt 12

{- | The outdoors t'Flags' bit lens. -}
{-# INLINE outdoors #-}
outdoors :: Lens' Flags Bool
outdoors = spellFlags % bitAt 13

{- | The ignoreMagic t'Flags' bit lens. -}
{-# INLINE ignoreMagic #-}
ignoreMagic :: Lens' Flags Bool
ignoreMagic = spellFlags % bitAt 14

{- | The ignoreSurge t'Flags' bit lens. -}
{-# INLINE ignoreSurge #-}
ignoreSurge :: Lens' Flags Bool
ignoreSurge = spellFlags % bitAt 15

{- | The notCombat t'Flags' bit lens. -}
{-# INLINE notCombat #-}
notCombat :: Lens' Flags Bool
notCombat = spellFlags % bitAt 16

{- | The targetInvis t'Flags' bit lens. -}
{-# INLINE targetInvis #-}
targetInvis :: Lens' Flags Bool
targetInvis = spellFlags % bitAt 24

{- | The castSilenced t'Flags' bit lens. -}
{-# INLINE castSilenced #-}
castSilenced :: Lens' Flags Bool
castSilenced = spellFlags % bitAt 25


{- | Decode a t'Flags'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Flags
decode = fmap toFlags Parsers.word32Le

{- | Encode a t'Flags'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Flags
encode = contramap fromFlags Serializers.word32Le
