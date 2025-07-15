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
    toSaveBonus,

    -- * Parsers.
    decodeSaveBonus,

    -- * Serializers.
    encodeSaveBonus,
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8, Int32)
import Data.Ix (Ix)
import Data.Void (absurd)
import Data.Word (Word8, Word32)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFunctor (ElementOf)
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Typeclasses.HasOffset (HasOffset)
import Trisagion.Typeclasses.Splittable (Splittable (PrefixOf))
import Trisagion.Typeclasses.Binary (Binary)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.ParseError (throwParseError, capture)
import Trisagion.Parsers.Word8 (word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Binary (word32Le)


{- | The t'SaveBonusError' type. -}
newtype SaveBonusError = SaveBonusError Int32
    deriving stock (Eq, Show)


{- | The @SaveBonus@ type. 

Refinement type to constrain the values of save bonuses to the interval @[-20 .. 20]@.
-}
newtype SaveBonus = SaveBonus Int8
    deriving stock (Eq, Ord, Ix, Show)

-- Instances.
instance Bounded SaveBonus where
    {-# INLINE minBound #-}
    minBound :: SaveBonus
    minBound = SaveBonus (-20)

    {-# INLINE maxBound #-}
    maxBound :: SaveBonus
    maxBound = SaveBonus 20


{- | Smart constructor for the t'SaveBonus' type.-}
{-# INLINE toSaveBonus #-}
toSaveBonus :: Int32 -> Maybe SaveBonus
toSaveBonus n = if -20 <= n && n <= 20 then Just $ SaveBonus (fromIntegral n) else Nothing


{- | Parse a t'SaveBonus'. -}
decodeSaveBonus
    :: (HasOffset s, Splittable s, MonoFoldable (PrefixOf s), ElementOf (PrefixOf s) ~ Word8)
    => Parser s (ParseError SaveBonusError) SaveBonus
decodeSaveBonus = capture $ do
    n <- first (fmap absurd) word32Le
    let m = fromIntegral n :: Int32
    maybe (throwParseError $ SaveBonusError m) pure (toSaveBonus m)


{- | Encode a t'SaveBonus' into a 'Word32'. -}
encodeSaveBonus :: Binary m => Serializer m SaveBonus
encodeSaveBonus = contramap unwrap Binary.word32Le
    where
        unwrap :: SaveBonus -> Word32
        unwrap (SaveBonus n) = fromIntegral n
