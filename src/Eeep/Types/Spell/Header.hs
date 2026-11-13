{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Spell.Header

The Spell t'Header' type.
-}

module Eeep.Types.Spell.Header (
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
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Void (absurd)
import Data.Word (Word8)

-- non-Hackage libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), review, coercedTo)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parsers.Split (skipExact, skipN)
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (nullN)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))

-- Package.
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (decode, encode)
import Eeep.Types.Shared.School (School)
import qualified Eeep.Types.Shared.School as School (decode, encode)
import Eeep.Types.Shared.Sectype (Sectype)
import qualified Eeep.Types.Shared.Sectype as Sectype (decode, encode)
import Eeep.Types.Shared.Strref (Strref)
import qualified Eeep.Types.Shared.Strref as Strref (decode, encode)
import Eeep.Types.Spell.SpType (SpType)
import qualified Eeep.Types.Spell.SpType as SpType (Error (..), decode, encode)
import Eeep.Types.Spell.Level (Level)
import qualified Eeep.Types.Spell.Level as Level (Error (..), decode, encode)
import Eeep.Types.Spell.Exclusion (Exclusion)
import qualified Eeep.Types.Spell.Exclusion as Exclusion (decode, encode)
import Eeep.Types.Spell.Flags (Flags)
import qualified Eeep.Types.Spell.Flags as Flags (decode, encode)
import Eeep.Types.Spell.Graphics (Graphics)
import qualified Eeep.Types.Spell.Graphics as Graphics (decode, encode)


{- | The t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = SpTypeError       {-# UNPACK #-} !Word8
    | LevelError        {-# UNPACK #-} !Word8
    | SoundError                       !ShortByteString
    | IconError                        !ShortByteString
    deriving stock (Eq, Ord, Generic, Show)


{- | The Spell @Header@ type. -}
data Header = Header {
    name        :: {-# UNPACK #-} !Strref,
    sound       :: {-# UNPACK #-} !Resref,
    flags       :: {-# UNPACK #-} !Flags,
    sptype      :: {-# UNPACK #-} !SpType,
    exclusion   :: {-# UNPACK #-} !Exclusion,
    graphics    :: {-# UNPACK #-} !Graphics,
    school      :: {-# UNPACK #-} !School,
    sectype     :: {-# UNPACK #-} !Sectype,
    level       :: {-# UNPACK #-} !Level,
    icon        :: {-# UNPACK #-} !Resref,
    description :: {-# UNPACK #-} !Strref
} deriving stock (Eq, Show, Generic)


{- | Parser for the Spell @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (InputError :+: Error) Header
decode = do
    name        <- first Left Strref.decode
    _           <- first absurd $ skipN 4
    sound       <- first Left Resref.decode
    flags       <- first Left Flags.decode
    sptype      <- first (fmap (review $ #_SpTypeError % coercedTo)) SpType.decode
    _           <- first absurd $ skipN 1
    exclusion   <- first Left Exclusion.decode
    graphics    <- first Left Graphics.decode
    _           <- first absurd $ skipN 1
    school      <- first Left School.decode
    _           <- first absurd $ skipN 1
    sectype     <- first Left Sectype.decode
    _           <- first absurd $ skipN 12
    level       <- first (fmap (review $ #_LevelError % coercedTo)) Level.decode
    _           <- first absurd $ skipN 5
    icon        <- first Left Resref.decode
    _           <- first absurd $ skipN 14
    description <- first Left Strref.decode
    _           <- first Left $ skipExact 16
    pure Header {..}

{- | Serializer for the Spell t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary s => Serializer s Header
encode
    =  contramap (^. #name) Strref.encode
    <> nullN 4
    <> contramap (^. #sound) Resref.encode
    <> contramap (^. #flags) Flags.encode
    <> contramap (^. #sptype) SpType.encode
    <> nullN 1
    <> contramap (^. #exclusion) Exclusion.encode
    <> contramap (^. #graphics) Graphics.encode
    <> nullN 1
    <> contramap (^. #school) School.encode
    <> nullN 1
    <> contramap (^. #sectype) Sectype.encode
    <> nullN 12
    <> contramap (^. #level) Level.encode
    <> nullN 5
    <> contramap (^. #icon) Resref.encode
    <> nullN 14
    <> contramap (^. #description) Strref.encode
    <> nullN 16
