{- |
Module: Eeep.Types.Opcode.SaveFlags

The @SaveFlags@ type.
-}

module Eeep.Types.Opcode.SaveFlags (
    -- * Types.
    SaveFlags,

    -- ** Isomorphisms.
    saveFlags,

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
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Lens', Iso', (%), coercedTo)

-- Package.
import Eeep.Utils.Bits (bitAt)


{- | The t'SaveFlags' type. -}
newtype SaveFlags = SaveFlags Word32
    deriving stock (Eq, Show)


{- | Prism for the t'SaveFlags' type. -}
{-# INLINABLE saveFlags #-}
saveFlags :: Iso' SaveFlags Word32
saveFlags = coercedTo


{- | The save vs. spells bit lens. -}
{-# INLINABLE spells #-}
spells :: Lens' SaveFlags Bool
spells = saveFlags % bitAt 0

{- | The save vs. breath bit lens. -}
{-# INLINABLE breath #-}
breath :: Lens' SaveFlags Bool
breath = saveFlags % bitAt 1

{- | The save vs. poison (paralyze) bit lens. -}
{-# INLINABLE poison #-}
poison :: Lens' SaveFlags Bool
poison = saveFlags % bitAt 2

{- | The save vs. wands bit lens. -}
{-# INLINABLE wands #-}
wands :: Lens' SaveFlags Bool
wands = saveFlags % bitAt 3

{- | The save vs. petrify bit lens. -}
{-# INLINABLE petrify #-}
petrify :: Lens' SaveFlags Bool
petrify = saveFlags % bitAt 4

{- | The ignore primary bit lens. -}
{-# INLINABLE ignorePrimary #-}
ignorePrimary :: Lens' SaveFlags Bool
ignorePrimary = saveFlags % bitAt 10

{- | The ignore secondary bit lens. -}
{-# INLINABLE ignoreSecondary #-}
ignoreSecondary :: Lens' SaveFlags Bool
ignoreSecondary = saveFlags % bitAt 11

{- | The bypass mirror image bit lens. -}
{-# INLINABLE bypassMI #-}
bypassMI :: Lens' SaveFlags Bool
bypassMI = saveFlags % bitAt 24
