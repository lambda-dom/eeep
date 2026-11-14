{- |
Module: Eeep.Types.Opcode.SaveBonus

The @SaveBonus@ type.
-}

module Eeep.Types.Opcode.SaveBonus (
    -- * Error types.
    Error (..),

    -- * Types.
    SaveBonus,

    -- ** Prisms.
    saveBonus,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int32)
import Data.Ix (Ix)

-- Libraries.
import Optics.Core (Prism', prism', review, preview)

-- non-Hackage libraries.
import Data.Enum.Optics (enum)
import Data.Int.Optics (int32ToInt)
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..), int32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..), int32Le)


{- | The t'SaveBonus' @Error@ type. -}
newtype Error = Error Int32
    deriving stock (Eq, Ord, Bounded, Show, Ix)
    deriving newtype Enum


{- | The @SaveBonus@ type. 

Refinement type to constrain the values of save bonuses to the interval @[-20 .. 20]@.
-}
newtype SaveBonus = SaveBonus Int32
    deriving stock (Eq, Ord, Ix)
    deriving newtype (Enum, Show)


-- Instances.
instance Bounded SaveBonus where
    {-# INLINE minBound #-}
    minBound :: SaveBonus
    minBound = SaveBonus (-20)

    {-# INLINE maxBound #-}
    maxBound :: SaveBonus
    maxBound = SaveBonus 20


{- | Prism for the @t'SaveBonus'@ type. -}
{-# INLINABLE saveBonus #-}
saveBonus :: Prism' Int32 SaveBonus
saveBonus = prism' construct match
    where
        construct :: SaveBonus -> Int32
        construct = fromIntegral @Int @Int32 . review enum

        match :: Int32 -> Maybe SaveBonus
        match = preview enum . review int32ToInt


{- | Decode a t'SaveBonus'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) SaveBonus
decode = validate (validator Error (preview saveBonus)) Parsers.int32Le

{- | Encode a t'SaveBonus'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s SaveBonus
encode = contramap (review saveBonus) Serializers.int32Le
