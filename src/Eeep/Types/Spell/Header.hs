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
import Data.Word (Word8)

-- non-Hackage libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), review, coercedTo)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (skipExact)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Typeclasses.Serializers.Binary (nulls)

-- Package.
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode, encode)
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
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Header
decode = do
    name        <- first Right Strref.decode
    _           <- first Right $ skipExact 4
    sound       <- first (first (review $ #_SoundError % coercedTo)) Resref.decode
    flags       <- first Right Flags.decode
    sptype      <- first (first (review $ #_SpTypeError % coercedTo)) SpType.decode
    _           <- first Right $ skipExact 1
    exclusion   <- first Right Exclusion.decode
    graphics    <- first Right Graphics.decode
    _           <- first Right $ skipExact 1
    school      <- first Right School.decode
    sectype     <- first Right Sectype.decode
    _           <- first Right $ skipExact 11
    level       <- first (first (review $ #_LevelError % coercedTo)) Level.decode
    _           <- first Right $ skipExact 5
    icon        <- first (first (review $ #_IconError % coercedTo)) Resref.decode
    _           <- first Right $ skipExact 14
    description <- first Right Strref.decode
    _           <- first Right $ skipExact 16
    pure Header {..}

{- | Serializer for the Spell t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s Header
encode
    =  contramap (^. #name) Strref.encode
    <> nulls 4
    <> contramap (^. #sound) Resref.encode
    <> contramap (^. #flags) Flags.encode
    <> contramap (^. #sptype) SpType.encode
    <> nulls 1
    <> contramap (^. #exclusion) Exclusion.encode
    <> contramap (^. #graphics) Graphics.encode
    <> nulls 1
    <> contramap (^. #school) School.encode
    <> contramap (^. #sectype) Sectype.encode
    <> nulls 11
    <> contramap (^. #level) Level.encode
    <> nulls 5
    <> contramap (^. #icon) Resref.encode
    <> nulls 14
    <> contramap (^. #description) Strref.encode
    <> nulls 16
