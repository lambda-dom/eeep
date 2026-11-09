{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Spell.Ability.Header

The spell ability t'Header' type.
-}

module Eeep.Types.Spell.Ability.Header (
    -- * Error types.
    Error (..),

    -- * Types.
    Header (..),

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Word (Word8, Word16)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), review, coercedTo, _1, _2)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (skipExact)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Typeclasses.Serializers.Binary (nulls)

-- Package.
import Eeep.Types.Shared.Block (Block)
import qualified Eeep.Types.Shared.Block as Block (encode)
import Eeep.Types.Shared.Projectile (Projectile)
import qualified Eeep.Types.Shared.Projectile as Projectile (decode, encode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode, encode)
import Eeep.Types.Shared.Location (Location)
import qualified Eeep.Types.Shared.Location as Location (Error (..), decode, encode)
import Eeep.Types.Spell.Ability.Target (Target)
import qualified Eeep.Types.Spell.Ability.Target as Target (Error (..), decode, encode)
import Eeep.Types.Spell.Ability.Level (Level)
import qualified Eeep.Types.Spell.Ability.Level as Level (Error (..), decode, encode)
import Eeep.Types.Spell.Ability.Speed (Speed)
import qualified Eeep.Types.Spell.Ability.Speed as Speed (Error (..), decode, encode)
import Eeep.Types.Spell.Ability.Range (Range)
import qualified Eeep.Types.Spell.Ability.Range as Range (decode, encode)


{- | The spell ability t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = LocationError     {-# UNPACK #-} !Word8
    | TargetError       {-# UNPACK #-} !Word8
    | LevelError        {-# UNPACK #-} !Word8
    | SpeedError        {-# UNPACK #-} !Word8
    | IconError                        !ShortByteString
    deriving stock (Eq, Ord, Generic, Show)


{- | The spell ability t'Header' type. -}
data Header = Header {
    location    :: {-# UNPACK #-} !Location,
    target      :: {-# UNPACK #-} !Target,
    count       :: {-# UNPACK #-} !Word8,
    range       :: {-# UNPACK #-} !Range,
    level       :: {-# UNPACK #-} !Level,
    speed       :: {-# UNPACK #-} !Speed,
    icon        :: {-# UNPACK #-} !Resref,
    projectile  :: {-# UNPACK #-} !Projectile
} deriving stock (Eq, Show, Generic)


{- | Parser for the spell ability @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode
    :: (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (Error :+: InputError) (Header, Int)
decode = do
    _          <- first Right $ skipExact 2
    location   <- first (first (review $ #_LocationError % coercedTo)) Location.decode
    _          <- first Right $ skipExact 1
    icon       <- first (first (review $ #_IconError % coercedTo)) Resref.decode
    target     <- first (first (review $ #_TargetError % coercedTo)) Target.decode
    count      <- first Right Parsers.word8
    range      <- first Right Range.decode
    level      <- first (first (review $ #_LevelError % coercedTo)) Level.decode
    _          <- first Right $ skipExact 1
    speed      <- first (first (review $ #_SpeedError % coercedTo)) Speed.decode
    _          <- first Right $ skipExact 11
    op_count   <- bimap Right (fromIntegral @Word16 @Int) Parsers.word16Le
    -- Note: opcode block offset is not needed, so skip two more bytes.
    _          <- first Right $ skipExact 6
    projectile <- first Right Projectile.decode
    pure (Header {..}, op_count)

{- | Serializer for the spell ability t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s (Header, Block)
encode
    =  nulls 2
    <> contramap (^. _1 % #location) Location.encode
    <> nulls 1
    <> contramap (^. _1 % #icon) Resref.encode
    <> contramap (^. _1 % #target) Target.encode
    <> contramap (^. _1 % #count) Serializers.word8
    <> contramap (^. _1 % #range) Range.encode
    <> contramap (^. _1 % #level) Level.encode
    <> nulls 1
    <> contramap (^. _1 % #speed) Speed.encode
    <> nulls 10
    <> contramap (^. _2) Block.encode
    <> nulls 4
    <> contramap (^. _1 % #projectile) Projectile.encode
