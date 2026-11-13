{- |
Module: Eeep.Utils.Enum

Bidirectional mapping (isomorphism) for dense enums.
-}
{-# LANGUAGE RecordWildCards #-}

module Eeep.Utils.Enum (
    -- * Types.
    BiIso,

    -- ** Constructors.
    fromAssociations,

    -- ** Prisms.
    biIso,
) where

-- Imports.
-- Base.
import Data.Ix (Ix (index))
import Data.Maybe (isJust)

-- Libraries.
import Data.Vector.Strict (Vector, (!), force, fromList)
import Optics.Core (Prism', prism')


{- | Isomorphism @a@ and a dense subrange of 'Int' implemented as a pair of strict vectors. -}
data BiIso a = BiIso {
    from :: !(Vector (Maybe a)),       -- ^ Implements @Int -> a@.
    to   :: !(Vector Int)              -- ^ Implements @a -> Int@.
} deriving stock (Eq, Show)


{- | Construct a t'BiIso' from an association list.

Assumes the list is ordered in @a@ and with no repetitions on 'Just'.
-}
fromAssociations :: forall a . [(Int, Maybe a)] -> BiIso a
fromAssociations pairs =
        let
            from = force . fromList . fmap snd $ pairs
            to   = force . fromList . fmap fst . filter (isJust . snd) $ pairs
        in
            BiIso {..}


{- | The derived prism from a t'BiIso'. -}
biIso :: forall a . (Ix a, Bounded a) => BiIso a -> Prism' Int a
biIso bis = prism' construct match
    where
        construct :: a -> Int
        construct x = let v = to bis in
            v ! (index (minBound, maxBound) x)

        match :: Int -> Maybe a
        match n = let v = from bis in
            if 0 <= n && n < length v then v ! n else Nothing
