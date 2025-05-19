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


{- | The 'SaveFlags' type.-}
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


{- | The save vs. spells bit flag. -}
{-# INLINE spells #-}
spells :: Lens' SaveFlags Bool
spells = flag 0

{- | The save vs. breath bit flag. -}
{-# INLINE breath #-}
breath :: Lens' SaveFlags Bool
breath = flag 1

{- | The save vs. poison (paralyze) bit flag. -}
{-# INLINE poison #-}
poison :: Lens' SaveFlags Bool
poison = flag 2

{- | The save vs. wands bit flag. -}
{-# INLINE wands #-}
wands :: Lens' SaveFlags Bool
wands = flag 3

{- | The save vs. petrify bit flag. -}
{-# INLINE petrify #-}
petrify :: Lens' SaveFlags Bool
petrify = flag 4

{- | The ignore primary bit flag. -}
{-# INLINE ignorePrimary #-}
ignorePrimary :: Lens' SaveFlags Bool
ignorePrimary = flag 10

{- | The ignore secondary bit flag. -}
{-# INLINE ignoreSecondary #-}
ignoreSecondary :: Lens' SaveFlags Bool
ignoreSecondary = flag 11

{- | The bypass mirror image bit flag. -}
{-# INLINE bypassMI #-}
bypassMI :: Lens' SaveFlags Bool
bypassMI = flag 24


{- | Constructor for the 'SaveFlags' type. -}
toSaveFlags :: Word32 -> SaveFlags
toSaveFlags n = SaveFlags $ n .&. mask
    where
        -- Mask to constrain values with only valid bits enabled.
        mask = 16780319
