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
import Data.Void (absurd)
import Data.Word (Word8, Word16)

-- Libraries.
import Optics.Core ((%), (^.), review, coercedTo, _1, _2)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable)
import Data.Word.Optics (word16ToInt)
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parsers.Split (skipN)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (nullN)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))

-- Package.
import Eeep.Types.Shared.Block (Block)
import qualified Eeep.Types.Shared.Block as Block (encode)
import Eeep.Types.Shared.Projectile (Projectile)
import qualified Eeep.Types.Shared.Projectile as Projectile (decode, encode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (decode, encode)
import Eeep.Types.Shared.Ability.Location (Location)
import qualified Eeep.Types.Shared.Ability.Location as Location (Error (..), decode, encode)
import Eeep.Types.Shared.Ability.Target (Target)
import qualified Eeep.Types.Shared.Ability.Target as Target (Error (..), decode, encode)
import Eeep.Types.Spell.Ability.Level (Level)
import qualified Eeep.Types.Spell.Ability.Level as Level (Error (..), decode, encode)
import Eeep.Types.Shared.Ability.Speed (Speed)
import qualified Eeep.Types.Shared.Ability.Speed as Speed (Error (..), decode, encode)


{- | The spell ability t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = LocationError     {-# UNPACK #-} !Word8
    | TargetError       {-# UNPACK #-} !Word8
    | LevelError        {-# UNPACK #-} !Word8
    | SpeedError        {-# UNPACK #-} !Word8
    deriving stock (Eq, Ord, Generic, Show)


{- | The spell ability t'Header' type. -}
data Header = Header {
    location    :: {-# UNPACK #-} !Location,
    target      :: {-# UNPACK #-} !Target,
    count       :: {-# UNPACK #-} !Word8,
    range       :: {-# UNPACK #-} !Word16,
    level       :: {-# UNPACK #-} !Level,
    speed       :: {-# UNPACK #-} !Speed,
    icon        :: {-# UNPACK #-} !Resref,
    projectile  :: {-# UNPACK #-} !Projectile
} deriving stock (Eq, Show, Generic)


{- | Parser for the spell ability @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode
    :: (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (InputError :+: Error) (Header, Int)
decode = do
    _          <- first absurd $ skipN 2
    location   <- first (fmap (review $ #_LocationError % coercedTo)) Location.decode
    _          <- first absurd $ skipN 1
    icon       <- first Left Resref.decode
    target     <- first (fmap (review $ #_TargetError % coercedTo)) Target.decode
    count      <- first Left Parsers.word8
    range      <- first Left Parsers.word16Le
    level      <- first (fmap (review $ #_LevelError % coercedTo)) Level.decode
    _          <- first absurd $ skipN 1
    speed      <- first (fmap (review $ #_SpeedError % coercedTo)) Speed.decode
    _          <- first absurd $ skipN 11
    op_count   <- bimap Left (review word16ToInt) Parsers.word16Le
    -- Note: opcode block offset is not needed, so skip two more bytes.
    _          <- first absurd $ skipN 6
    projectile <- first Left Projectile.decode
    pure (Header {..}, op_count)

{- | Serializer for the spell ability t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary s => Serializer s (Header, Block)
encode
    =  nullN 2
    <> contramap (^. _1 % #location) Location.encode
    <> nullN 1
    <> contramap (^. _1 % #icon) Resref.encode
    <> contramap (^. _1 % #target) Target.encode
    <> contramap (^. _1 % #count) Serializers.word8
    <> contramap (^. _1 % #range) Serializers.word16Le
    <> contramap (^. _1 % #level) Level.encode
    <> nullN 1
    <> contramap (^. _1 % #speed) Speed.encode
    <> nullN 11
    <> contramap (^. _2) Block.encode
    <> nullN 4
    <> contramap (^. _1 % #projectile) Projectile.encode
