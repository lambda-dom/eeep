{- |
Module: Eeep.Types.Opcode.Flags

The @Flags@ type.
-}

module Eeep.Types.Opcode.Flags (
    -- * Types.
    Flags,

    -- ** Constructors and eliminators.
    toFlags,
    fromFlags,

    -- ** Lenses.
    spells,
    breath,
    poison,
    wands,
    petrify,
    ignorePrimary,
    ignoreSecondary,
    bypassMI,

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
import Optics.Core (Lens', Iso', (%), coercedTo, view)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The t'Flags' type.

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
        mask = 16780319

{- | Eliminator for the t'Flags' type. -}
{-# INLINE fromFlags #-}
fromFlags :: Flags -> Word32
fromFlags = view saveFlags


-- | Isomorphism for the t'Flags' type. Not exported.
{-# INLINE saveFlags #-}
saveFlags :: Iso' Flags Word32
saveFlags = coercedTo


{- | The save vs. spells bit lens. -}
{-# INLINE spells #-}
spells :: Lens' Flags Bool
spells = saveFlags % bitAt 0

{- | The save vs. breath bit lens. -}
{-# INLINE breath #-}
breath :: Lens' Flags Bool
breath = saveFlags % bitAt 1

{- | The save vs. poison (paralyze) bit lens. -}
{-# INLINE poison #-}
poison :: Lens' Flags Bool
poison = saveFlags % bitAt 2

{- | The save vs. wands bit lens. -}
{-# INLINE wands #-}
wands :: Lens' Flags Bool
wands = saveFlags % bitAt 3

{- | The save vs. petrify bit lens. -}
{-# INLINE petrify #-}
petrify :: Lens' Flags Bool
petrify = saveFlags % bitAt 4

{- | The ignore primary bit lens. -}
{-# INLINE ignorePrimary #-}
ignorePrimary :: Lens' Flags Bool
ignorePrimary = saveFlags % bitAt 10

{- | The ignore secondary bit lens. -}
{-# INLINE ignoreSecondary #-}
ignoreSecondary :: Lens' Flags Bool
ignoreSecondary = saveFlags % bitAt 11

{- | The bypass mirror image bit lens. -}
{-# INLINE bypassMI #-}
bypassMI :: Lens' Flags Bool
bypassMI = saveFlags % bitAt 24


{- | Decode a t'Flags'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Flags
decode = fmap toFlags Parsers.word32Le

{- | Encode a t'Flags'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Flags
encode = contramap fromFlags Serializers.word32Le
