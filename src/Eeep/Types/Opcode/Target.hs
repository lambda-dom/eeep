{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Eeep.Types.Opcode.Target

The @Target@ type.
-}

module Eeep.Types.Opcode.Target (
    -- * Error types.
    TargetError (..),

    -- * Types.
    Target (..),
) where

-- Imports.
-- Base.
import Data.Bifunctor (Bifunctor(..))
import Data.Ix (Ix)
import Data.Word (Word8)

-- Libraries.
import Control.Monad.Except (MonadError(throwError))

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Parsers.Source as Source (one)
import Trisagion.Serializer (Serializer, embed)

-- Package.
import Eeep.Typeclasses.Binary (Reader (..), Writer (..))
import Eeep.Utils.Enum (maybeEnum)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant(..))


{- | The t'TargetError' error type. -}
newtype TargetError = TargetError Word8
    deriving stock (Eq, Ord, Bounded, Show)
    deriving newtype Enum


{- | The @Target@ enumeration type. -}
data Target
    = None
    | Self
    | Preset
    | Party
    | Area
    | NotParty
    | CasterGroup
    | TargetGroup
    | NotSelf
    | Original
    deriving stock (Eq, Ord, Enum, Bounded, Ix, Show)


-- Instances.
instance Source Word8 s => Reader s (TargetError :+: InputError) Target where
    {-# INLINE parser #-}
    parser :: Parser s (TargetError :+: InputError) Target
    parser = do
        n <- first Right Source.one
        case maybeEnum n :: Maybe Target of
            Nothing -> throwError $ Left (TargetError n)
            Just y  -> pure y

instance Sink Word8 ByteString s => Writer s Target where
    {-# INLINE serializer #-}
    serializer :: Serializer s Target
    serializer = contramap (fromIntegral . fromEnum) one
        where
            one = embed single
