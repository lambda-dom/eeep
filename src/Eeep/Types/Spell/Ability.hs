{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Spell.Ability

The spell t'Ability' type.
-}

module Eeep.Types.Spell.Ability (
    -- * Error types.
    Error (..),

    -- * Types.
    Ability (..),

    -- ** Parsers and serializers.
    decode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Word (Word8, Word16)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), review, coercedTo)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (skipExact)

-- Package.
import Eeep.Types.Shared.Projectile (Projectile)
import qualified Eeep.Types.Shared.Projectile as Projectile (decode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode)
import Eeep.Types.Spell.Ability.Location (Location)
import qualified Eeep.Types.Spell.Ability.Location as Location (Error (..), decode)
import Eeep.Types.Spell.Ability.Target (Target)
import qualified Eeep.Types.Spell.Ability.Target as Target (Error (..), decode)
import Eeep.Types.Spell.Ability.Level (Level)
import qualified Eeep.Types.Spell.Ability.Level as Level (Error (..), decode)
import Eeep.Types.Spell.Ability.Time (Time)
import qualified Eeep.Types.Spell.Ability.Time as Time (Error (..), decode)
import Eeep.Types.Spell.Ability.Range (Range)
import qualified Eeep.Types.Spell.Ability.Range as Range (decode)


{- | The t'Ability' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = LocationError     {-# UNPACK #-} !Word8
    | TargetError       {-# UNPACK #-} !Word8
    | LevelError        {-# UNPACK #-} !Word8
    | TimeError         {-# UNPACK #-} !Word8
    | IconError                        !ShortByteString
    deriving stock (Eq, Ord, Generic, Show)


{- | The spell t'Ability' type. -}
data Ability = Ability {
    location    :: {-# UNPACK #-} !Location,
    target      :: {-# UNPACK #-} !Target,
    count       :: {-# UNPACK #-} !Word8,
    range       :: {-# UNPACK #-} !Range,
    level       :: {-# UNPACK #-} !Level,
    time        :: {-# UNPACK #-} !Time,
    icon        :: {-# UNPACK #-} !Resref,
    projectile  :: {-# UNPACK #-} !Projectile
} deriving stock (Eq, Show, Generic)


{- | Parser for the Spell @t'Ability'@ type. -}
{-# INLINEABLE decode #-}
decode
    :: (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (Error :+: InputError) (Ability, Word16, Word16)
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
    time       <- first (first (review $ #_TimeError % coercedTo)) Time.decode
    _          <- first Right $ skipExact 11
    opcount    <- first Right Parsers.word16Le
    opoffset   <- first Right Parsers.word16Le
    _          <- first Right $ skipExact 4
    projectile <- first Right Projectile.decode
    pure (Ability {..}, opcount, opoffset)
