{- |
Module: Eeep.Types.Opcode.SaveFlags

The @SaveFlags@ type.
-}

module Eeep.Types.Opcode.SaveFlags (
    -- * Types.
    SaveFlags,

    -- ** Constructors and eliminators.
    toSaveFlags,
    fromSaveFlags,

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
import Data.Bits ((.&.))
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Lens', Iso', (%), coercedTo, view)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt)


{- | The t'SaveFlags' type. -}
newtype SaveFlags = SaveFlags Word32
    deriving stock (Eq, Show)


{- | Smart constructor for the t'SaveFlags' type.

note(s)

    * The function performs normalization so it is *not* an inverse of 'fromSaveFlags', only a
    right-inverse.
-}
{-# INLINE toSaveFlags #-}
toSaveFlags :: Word32 -> SaveFlags
toSaveFlags n = SaveFlags $ n .&. mask
    where
        mask :: Word32
        mask = 16780319

{- | Eliminator for the t'SaveFlags' type. -}
{-# INLINE fromSaveFlags #-}
fromSaveFlags :: SaveFlags -> Word32
fromSaveFlags = view saveFlags


{- | Prism for the t'SaveFlags' type.

note(s):

    * Not exported as the constructor does not perform normalization.
-}
{-# INLINE saveFlags #-}
saveFlags :: Iso' SaveFlags Word32
saveFlags = coercedTo


{- | The save vs. spells bit lens. -}
{-# INLINE spells #-}
spells :: Lens' SaveFlags Bool
spells = saveFlags % bitAt 0

{- | The save vs. breath bit lens. -}
{-# INLINE breath #-}
breath :: Lens' SaveFlags Bool
breath = saveFlags % bitAt 1

{- | The save vs. poison (paralyze) bit lens. -}
{-# INLINE poison #-}
poison :: Lens' SaveFlags Bool
poison = saveFlags % bitAt 2

{- | The save vs. wands bit lens. -}
{-# INLINE wands #-}
wands :: Lens' SaveFlags Bool
wands = saveFlags % bitAt 3

{- | The save vs. petrify bit lens. -}
{-# INLINE petrify #-}
petrify :: Lens' SaveFlags Bool
petrify = saveFlags % bitAt 4

{- | The ignore primary bit lens. -}
{-# INLINE ignorePrimary #-}
ignorePrimary :: Lens' SaveFlags Bool
ignorePrimary = saveFlags % bitAt 10

{- | The ignore secondary bit lens. -}
{-# INLINE ignoreSecondary #-}
ignoreSecondary :: Lens' SaveFlags Bool
ignoreSecondary = saveFlags % bitAt 11

{- | The bypass mirror image bit lens. -}
{-# INLINE bypassMI #-}
bypassMI :: Lens' SaveFlags Bool
bypassMI = saveFlags % bitAt 24
