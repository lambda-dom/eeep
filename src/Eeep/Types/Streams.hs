{- |
Module: Eeep.Types.Streams

Wrapper around a 'Streamable' adding an offset to track current position.
-}

module Eeep.Types.Streams (
    -- * Types.
    Stream,

    -- * Constructors.
    initialize,

    -- * Getters.
    position,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor (..))

-- Libraries.
import Data.MonoTraversable (MonoFunctor (..),  MonoFoldable (..), Element)

-- Libraries.
import Bins.Typeclasses.Streamable (Streamable (..))


{- | Wrapper around a 'Streamable' adding an offset to track current position. -}
data Stream s = Stream !Word !s
    deriving stock (Eq, Show)

-- Instances.
type instance Element (Stream s) = Element s
instance MonoFunctor s => MonoFunctor (Stream s) where
    omap :: (Element (Stream s) -> Element (Stream s)) -> Stream s -> Stream s
    omap f (Stream n xs) = Stream n (omap f xs)

instance (MonoFoldable s) => MonoFoldable (Stream s) where
    ofoldMap :: Monoid m => (Element (Stream s) -> m) -> Stream s -> m
    ofoldMap f (Stream _ xs) = ofoldMap f xs

    ofoldr :: (Element (Stream s) -> a -> a) -> a -> Stream s -> a
    ofoldr f x (Stream _ xs) = ofoldr f x xs

    ofoldl' :: (a -> Element (Stream s) -> a) -> a -> Stream s -> a
    ofoldl' f x (Stream _ xs) = ofoldl' f x xs

    ofoldr1Ex
        :: (Element (Stream s) -> Element (Stream s) -> Element (Stream s))
        -> Stream s
        -> Element (Stream s)
    ofoldr1Ex f (Stream _ xs) = ofoldr1Ex f xs

    ofoldl1Ex'
        :: (Element (Stream s) -> Element (Stream s) -> Element (Stream s))
        -> Stream s
        -> Element (Stream s)
    ofoldl1Ex' f (Stream _ xs) = ofoldl1Ex' f xs

instance Streamable s => Streamable (Stream s) where
    getOne :: Stream s -> Maybe (Element (Stream s), Stream s)
    getOne (Stream n xs) = second (Stream (succ n)) <$> getOne xs


{- | Construct a t'Stream' from a 'Streamable'. -}
initialize :: s -> Stream s
initialize = Stream 0

{- | Return the current position of the t'Stream'. -}
position :: Stream s -> Word
position (Stream n _) = n
