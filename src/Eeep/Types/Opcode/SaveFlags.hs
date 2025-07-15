{- |
Module: Eeep.Types.Opcode.SaveFlags

The @SaveFlags@ type.
-}

module Eeep.Types.Opcode.SaveFlags (
    -- * Types.
    SaveFlags,

    -- ** Constructors.
    toSaveFlags,

    -- ** Lenses.
    spells,
    breath,
    poison,
    wands,
    petrify,
    ignorePrimary,
    ignoreSecondary,
    bypassMI,

    -- * Parsers.
    decodeSaveFlags,

    -- * Serializers.
    encodeSaveFlags,
) where

-- Imports.
-- Base.
import Data.Bits ((.&.), (.|.), bit, zeroBits)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8, Word32)

-- Libraries.
import Optics.Core (Lens', lens)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (capture)
import Trisagion.Parsers.Streamable (InputError)
import Trisagion.Parsers.Word8 (word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Binary (word32Le)


{- | The t'SaveFlags' type.-}
newtype SaveFlags = SaveFlags Word32
    deriving stock (Eq, Show)


{- | Generic bit lens. -}
flag :: Int -> Lens' SaveFlags Bool
flag n = lens project update
    where
        project :: SaveFlags -> Bool
        project (SaveFlags ns) = ns .&. bit n /= zeroBits

        update :: SaveFlags -> Bool -> SaveFlags
        update r@(SaveFlags ns) b = if b then SaveFlags (ns .|. bit n) else r


{- | The save vs. spells bit lens. -}
{-# INLINE spells #-}
spells :: Lens' SaveFlags Bool
spells = flag 0

{- | The save vs. breath bit lens. -}
{-# INLINE breath #-}
breath :: Lens' SaveFlags Bool
breath = flag 1

{- | The save vs. poison (paralyze) bit lens. -}
{-# INLINE poison #-}
poison :: Lens' SaveFlags Bool
poison = flag 2

{- | The save vs. wands bit lens. -}
{-# INLINE wands #-}
wands :: Lens' SaveFlags Bool
wands = flag 3

{- | The save vs. petrify bit lens. -}
{-# INLINE petrify #-}
petrify :: Lens' SaveFlags Bool
petrify = flag 4

{- | The ignore primary bit lens. -}
{-# INLINE ignorePrimary #-}
ignorePrimary :: Lens' SaveFlags Bool
ignorePrimary = flag 10

{- | The ignore secondary bit lens. -}
{-# INLINE ignoreSecondary #-}
ignoreSecondary :: Lens' SaveFlags Bool
ignoreSecondary = flag 11

{- | The bypass mirror image bit lens. -}
{-# INLINE bypassMI #-}
bypassMI :: Lens' SaveFlags Bool
bypassMI = flag 24


{- | Constructor for the t'SaveFlags' type. -}
toSaveFlags :: Word32 -> SaveFlags
toSaveFlags n = SaveFlags $ n .&. mask
    where
        -- Mask to constrain values with only valid bits enabled.
        mask = 16780319


{- | Decode an opcode's t'SaveFlags'. -}
decodeSaveFlags
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s InputError SaveFlags
decodeSaveFlags = capture . fmap toSaveFlags $ word32Le


{- | Encode a t'SaveFlags' into a 'Word32'. -}
encodeSaveFlags :: Binary m => Serializer m SaveFlags
encodeSaveFlags = contramap unwrap Binary.word32Le
    where
        unwrap :: SaveFlags -> Word32
        unwrap (SaveFlags n) = n
