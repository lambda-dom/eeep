{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Shared.Strength

The pair @Strength@ type to keep track of parsed opcode blocks.
-}

module Eeep.Types.Shared.Strength (
    -- * Error types.
    Error (..),

    -- * Types.
    Strength (..),

    -- ** Prisms.
    strength,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', (%), coercedTo, review, (^.), prism')

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)

-- Package.
import Eeep.Types.Shared.Attribute (Attribute)
import qualified Eeep.Types.Shared.Attribute as Attribute (Error (..), decode, encode, attribute)
import Eeep.Types.Shared.Percentage (Percentage)
import qualified Eeep.Types.Shared.Percentage as Percentage (Error (..), decode, encode, percentage)


{- | The t'Strength' @Error@ type. -}
data Error
    = AttributeError  {-# UNPACK #-} !Word8
    | PercentageError {-# UNPACK #-} !Word8
    deriving stock (Eq, Ord, Show, Generic)


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


{- | Decode a t'Strength'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Strength
decode = do
    attribute  <- first (first (review $ #_AttributeError % coercedTo)) Attribute.decode
    percentage <- first (first (review $ #_PercentageError % coercedTo)) Percentage.decode
    pure $ Strength {..}

{- | Encode a t'Strength'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Strength
encode
    =  contramap (^. #attribute) Attribute.encode
    <> contramap (^. #percentage) Percentage.encode
