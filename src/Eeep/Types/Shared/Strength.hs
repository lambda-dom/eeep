{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Shared.Strength

The pair @Strength@ type to keep track of parsed opcode blocks.
-}

module Eeep.Types.Shared.Strength (
    -- * Types.
    Strength (..),

    -- ** Prisms.
    strength,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)

-- Libraries.
import Optics.Core (Prism', review, prism')

-- Package.
import Eeep.Types.Shared.Attribute (Attribute)
import qualified Eeep.Types.Shared.Attribute as Attribute (attribute)
import Eeep.Types.Shared.Percentage (Percentage)
import qualified Eeep.Types.Shared.Percentage as Percentage (percentage)


{- | The @Strength@ type. -}
data Strength = Strength {
    attribute  :: {-# UNPACK #-} !Attribute,
    percentage :: {-# UNPACK #-} !Percentage
} deriving stock (Eq, Ord, Bounded, Show, Generic)


{- | Prism for the t'Strength' type. -}
{-# INLINE strength #-}
strength :: Prism' (Attribute, Percentage) Strength
strength = prism' construct match
    where
        construct :: Strength -> (Attribute, Percentage)
        construct (Strength a p) = (a, p)

        match :: (Attribute, Percentage) -> Maybe Strength
        match (a, p) =
            if review Attribute.attribute a /= 18 && review Percentage.percentage p /= 0
            then Nothing
            else Just $ Strength a p
