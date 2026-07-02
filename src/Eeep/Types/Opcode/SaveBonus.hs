{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.SaveBonus

The @SaveBonus@ type.
-}

module Eeep.Types.Opcode.SaveBonus (
    -- * Error types.
    SaveBonusError (..),

    -- * Types.
    SaveBonus,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant(..))
import Data.Int (Int32)
import Data.Ix (Ix)
import Data.Word (Word8)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary, word32Le)
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Binary as Serializers (Binary, word32Le)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))
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

instance (Source Word8 s, Parsers.Binary b s) => Reader s (SaveBonusError :+: InputError) SaveBonus where
    {-# INLINE parser #-}
    parser :: Parser s (SaveBonusError :+: InputError) SaveBonus
    parser = validate (eitherEnum SaveBonusError) (fmap fromIntegral Parsers.word32Le)

instance (Sink Word8 b s, Serializers.Binary b s) => Writer b s SaveBonus where
    {-# INLINE serializer #-}
    serializer :: Serializer s SaveBonus
    serializer = contramap (fromIntegral . unwrap) Serializers.word32Le
        where
            unwrap :: SaveBonus -> Int32
            unwrap (SaveBonus n) = n
