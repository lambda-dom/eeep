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
) where

-- Imports.
-- Base.
import Data.Bits ((.&.), (.|.), bit, zeroBits)
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Lens', lens)


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
