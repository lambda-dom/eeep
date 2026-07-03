{- |
Module: Eeep.Types.Opcode.SaveFlags

The @SaveFlags@ type.
-}

module Eeep.Types.Opcode.SaveFlags (
    -- * Types.
    SaveFlags,

    -- ** Lenses.
    spells,
    breath,
    poison,
    wands,
    petrify,
    ignorePrimary,
    ignoreSecondary,
    bypassMI,

    -- ** Constructors.
    saveFlags,

    -- ** Parsers and serializers.
    encodeSaveFlags,
    decodeSaveFlags,
) where

-- Imports.
-- Base.
import Data.Bits (Bits (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Lens', (%))
import Optics.Iso (Iso', coercedTo)

-- non-Hackage libraries.
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary, word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word32Le)

-- Package.
import Eeep.Utils.Bits (bitAt)


{- | The t'SaveFlags' type.-}
newtype SaveFlags = SaveFlags Word32
    deriving stock (Eq, Show)


{- | Specialized version of 'coercedTo' serving as a type annotation. -}
coerce :: Iso' SaveFlags Word32
coerce = coercedTo


{- | The save vs. spells bit lens. -}
{-# INLINE spells #-}
spells :: Lens' SaveFlags Bool
spells = coerce % bitAt 0

{- | The save vs. breath bit lens. -}
{-# INLINE breath #-}
breath :: Lens' SaveFlags Bool
breath = coerce % bitAt 1

{- | The save vs. poison (paralyze) bit lens. -}
{-# INLINE poison #-}
poison :: Lens' SaveFlags Bool
poison = coerce % bitAt 2

{- | The save vs. wands bit lens. -}
{-# INLINE wands #-}
wands :: Lens' SaveFlags Bool
wands = coerce % bitAt 3

{- | The save vs. petrify bit lens. -}
{-# INLINE petrify #-}
petrify :: Lens' SaveFlags Bool
petrify = coerce % bitAt 4

{- | The ignore primary bit lens. -}
{-# INLINE ignorePrimary #-}
ignorePrimary :: Lens' SaveFlags Bool
ignorePrimary = coerce % bitAt 10

{- | The ignore secondary bit lens. -}
{-# INLINE ignoreSecondary #-}
ignoreSecondary :: Lens' SaveFlags Bool
ignoreSecondary = coerce % bitAt 11

{- | The bypass mirror image bit lens. -}
{-# INLINE bypassMI #-}
bypassMI :: Lens' SaveFlags Bool
bypassMI = coerce % bitAt 24


{- | Constructor for the t'SaveFlags' type. -}
{-# INLINE saveFlags #-}
saveFlags :: Word32 -> SaveFlags
saveFlags n = SaveFlags $ n .&. mask
    where
        -- Mask to normalize values to have only valid bits enabled.
        mask = 16780319


{- | Default parser for t'SaveFlags'. -}
{-# INLINE encodeSaveFlags #-}
encodeSaveFlags :: Parsers.Binary b s => Parser s InputError SaveFlags
encodeSaveFlags = fmap SaveFlags Parsers.word32Le

{- | Default serializer for t'SaveFlags'. -}
{-# INLINE decodeSaveFlags #-}
decodeSaveFlags :: Serializers.Binary b s => Serializer s SaveFlags
decodeSaveFlags = contramap unwrap Serializers.word32Le
    where
        unwrap :: SaveFlags -> Word32
        unwrap (SaveFlags n) = n
