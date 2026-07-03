{- |
Module: Eeep.Types.Opcode.SaveBonus

The @SaveBonus@ type.
-}

module Eeep.Types.Opcode.SaveBonus (
    -- * Error types.
    SaveBonusError (..),

    -- * Types.
    SaveBonus,

    -- ** Constructors.
    saveBonus,

    -- ** Parsers and serializers.
    encodeSaveBonus,
    decodeSaveBonus,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant(..))
import Data.Int (Int32)
import Data.Ix (Ix)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary, word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word32Le)

-- Package.
import Eeep.Utils.Enum (eitherEnum)


{- | The t'SaveBonusError' type. -}
newtype SaveBonusError = SaveBonusError Int32
    deriving stock (Eq, Show)


{- | The @SaveBonus@ type. 

Refinement type to constrain the values of save bonuses to the interval @[-20 .. 20]@.
-}
newtype SaveBonus = SaveBonus Int32
    deriving stock (Eq, Ord, Ix, Show)
    deriving newtype Enum


-- Instances.
instance Bounded SaveBonus where
    {-# INLINE minBound #-}
    minBound :: SaveBonus
    minBound = SaveBonus (-20)

    {-# INLINE maxBound #-}
    maxBound :: SaveBonus
    maxBound = SaveBonus 20


{- | Smart constructor for the @t'SaveBonus'@ type. -}
{-# INLINE saveBonus #-}
saveBonus :: Int32 -> SaveBonusError :+: SaveBonus
saveBonus n = eitherEnum (SaveBonusError n) n


{- | Default parser for t'SaveBonus'. -}
{-# INLINE encodeSaveBonus #-}
encodeSaveBonus :: Parsers.Binary b s => Parser s (SaveBonusError :+: InputError) SaveBonus
encodeSaveBonus = validate saveBonus (fmap fromIntegral Parsers.word32Le)

{- | Default serializer for t'SaveBonus'. -}
{-# INLINE decodeSaveBonus #-}
decodeSaveBonus :: Serializers.Binary b s => Serializer s SaveBonus
decodeSaveBonus = contramap (fromIntegral . unwrap) Serializers.word32Le
    where
        unwrap :: SaveBonus -> Int32
        unwrap (SaveBonus n) = n
