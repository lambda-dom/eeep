{- |
Module: Eeep.Spell.Exclusion

The Exclusion flags of the spell header.
-}

module Eeep.Types.Spell.Exclusion (
    -- * Types.
    Exclusion,

    -- * Constructors and eliminators.
    toExclusion,
    fromExclusion,

    -- ** Exclusion bit lenses.
    chaotic,
    evil,
    good,
    geneutral,
    lawful,
    lcneutral,
    abjurer,
    conjurer,
    diviner,
    enchanter,
    illusionist,
    invoker,
    necromancer,
    transmuter,
    wild,
    divine,
    nature,
) where

-- Imports.
-- Base.
import Data.Bits ((.&.))
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', Lens', (%), coercedTo, view)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt)


{- | The Exclusion flags type.

The meaning of the bits depends on the spell type.
-}
newtype Exclusion = Exclusion Word32
    deriving stock (Eq, Show)


{- | Smart constructor for the t'Exclusion' type.

note(s)

    * The constructor performs normalization so it is *not* an inverse of 'fromExclusion', only a
    right-inverse. By the same token, it is not part of a 'Optics.Core.Prism'.
-}
{-# INLINE toExclusion #-}
toExclusion :: Word32 -> Exclusion
toExclusion n = Exclusion $ n .&. mask
    where
        mask :: Word32
        mask = 3221258239

{- | Eliminator for the t'Exclusion' type. -}
{-# INLINE fromExclusion #-}
fromExclusion :: Exclusion -> Word32
fromExclusion = view exclusion


-- | Prism for the t'Exclusion' type. Not exported.
{-# INLINE exclusion #-}
exclusion :: Iso' Exclusion Word32
exclusion = coercedTo


{- | The chaotic t'Exclusion' bit lens. -}
{-# INLINE chaotic #-}
chaotic :: Lens' Exclusion Bool
chaotic = exclusion % bitAt 0

{- | The evil t'Exclusion' bit lens. -}
{-# INLINE evil #-}
evil :: Lens' Exclusion Bool
evil = exclusion % bitAt 1

{- | The good t'Exclusion' bit lens. -}
{-# INLINE good #-}
good :: Lens' Exclusion Bool
good = exclusion % bitAt 2

{- | The geneutral t'Exclusion' bit lens. -}
{-# INLINE geneutral #-}
geneutral :: Lens' Exclusion Bool
geneutral = exclusion % bitAt 3

{- | The lawful t'Exclusion' bit lens. -}
{-# INLINE lawful #-}
lawful :: Lens' Exclusion Bool
lawful = exclusion % bitAt 4

{- | The lcneutral t'Exclusion' bit lens. -}
{-# INLINE lcneutral #-}
lcneutral :: Lens' Exclusion Bool
lcneutral = exclusion % bitAt 5

{- | The abjurer t'Exclusion' bit lens. -}
{-# INLINE abjurer #-}
abjurer :: Lens' Exclusion Bool
abjurer = exclusion % bitAt 6

{- | The conjurer t'Exclusion' bit lens. -}
{-# INLINE conjurer #-}
conjurer :: Lens' Exclusion Bool
conjurer = exclusion % bitAt 7

{- | The diviner t'Exclusion' bit lens. -}
{-# INLINE diviner #-}
diviner :: Lens' Exclusion Bool
diviner = exclusion % bitAt 8

{- | The enchanter t'Exclusion' bit lens. -}
{-# INLINE enchanter #-}
enchanter :: Lens' Exclusion Bool
enchanter = exclusion % bitAt 9

{- | The illusionist t'Exclusion' bit lens. -}
{-# INLINE illusionist #-}
illusionist :: Lens' Exclusion Bool
illusionist = exclusion % bitAt 10

{- | The invoker t'Exclusion' bit lens. -}
{-# INLINE invoker #-}
invoker :: Lens' Exclusion Bool
invoker = exclusion % bitAt 11

{- | The necromancer t'Exclusion' bit lens. -}
{-# INLINE necromancer #-}
necromancer :: Lens' Exclusion Bool
necromancer = exclusion % bitAt 12

{- | The transmuter t'Exclusion' bit lens. -}
{-# INLINE transmuter #-}
transmuter :: Lens' Exclusion Bool
transmuter = exclusion % bitAt 13

{- | The wild t'Exclusion' bit lens. -}
{-# INLINE wild #-}
wild :: Lens' Exclusion Bool
wild = exclusion % bitAt 14

{- | The divine t'Exclusion' bit lens. -}
{-# INLINE divine #-}
divine :: Lens' Exclusion Bool
divine = exclusion % bitAt 30

{- | The nature t'Exclusion' bit lens. -}
{-# INLINE nature #-}
nature :: Lens' Exclusion Bool
nature = exclusion % bitAt 31
