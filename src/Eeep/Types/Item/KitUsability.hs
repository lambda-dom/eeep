{- |
Module: Eeep.Types.Item.KitUsability

The @KitUsability@ type for the per kit usability flags field of the main item header.
-}

module Eeep.Types.Item.KitUsability (
    -- * Types.
    KitUsability,

    -- ** Isomorphisms.
    kitUsability,
    exclusionBytes,

    -- ** Bit lenses.
    talos,
) where

-- Imports.
-- Base.
import Data.Word (Word32, Word8)

-- Libraries.
import Optics.Core (Iso', Lens', (%), coercedTo)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt, word32BytesLe)


{- | The @KitUsability@ type for the per race/class usability flags field of the main item header.

note(s):

    * The 'Ord' and 'Bounded' instances have no significance and are present merely to be able to
    use t'KitUsability
' values as keys in maps, etc.
-}
newtype KitUsability = KitUsability Word32
    deriving stock (Eq, Ord, Bounded, Show)


{- | Isomorphism for the t'KitUsability' type. -}
{-# INLINE kitUsability #-}
kitUsability :: Iso' KitUsability Word32
kitUsability = coercedTo

{- | Isomorphism between the t'KitUsability' type and the 4 exclusion bytes. -}
{-# INLINE exclusionBytes #-}
exclusionBytes :: Iso' KitUsability (Word8, Word8, Word8, Word8)
exclusionBytes = kitUsability % word32BytesLe


{- | The Talos Cleric t'KitUsability' bit lens. -}
{-# INLINE talos #-}
talos :: Lens' KitUsability Bool
talos = kitUsability % bitAt 0
