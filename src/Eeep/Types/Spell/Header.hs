{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Spell

The Spell t'Header' type.
-}

module Eeep.Types.Spell.Header (
    -- * Error types.
    Error (..),

    -- * Types.
    Header (..),

    -- * Parsers and serializers.
    decode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Word (Word8)

-- non-Hackage libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), review, coercedTo)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip, validate)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (takeExact, skipExact)

-- Package.
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode)
import Eeep.Types.Shared.School (School)
import qualified Eeep.Types.Shared.School as School (decode)
import Eeep.Types.Shared.Sectype (Sectype)
import Eeep.Types.Shared.Strref (Strref)
import qualified Eeep.Types.Shared.Strref as Strref (decode)
import Eeep.Types.Spell.SpType (SpType)
import qualified Eeep.Types.Spell.SpType as SpType (Error (..), decode)
import Eeep.Types.Spell.Level (Level)
import qualified Eeep.Types.Spell.Level as Level (Error (..), decode)
import Eeep.Types.Spell.Exclusion (Exclusion)
import qualified Eeep.Types.Spell.Exclusion as Exclusion (decode)
import Eeep.Types.Spell.Flags (Flags)
import qualified Eeep.Types.Spell.Flags as Flags (decode)
import Eeep.Types.Spell.Graphics (Graphics)
import qualified Eeep.Types.Spell.Graphics as Graphics (decode)


{- | The t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error b
    = SignatureError                   !b
    | SpTypeError       {-# UNPACK #-} !Word8
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


-- | Parser for the Spell @t'Header'@ signature. Not exported.
{-# INLINE signature #-}
signature
    :: forall b s . (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (Error b :+: InputError) ()
signature = skip $ validate v (takeExact 8)
    where
        v :: b -> Error b :+: b
        v xs =
            if fmap (fromIntegral @Int @Word8 . ord) "SPL V1  " /= monotoList xs
            then Left $ SignatureError xs
            else Right xs


{- | Parser for the Spell @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error b :+: InputError) Header
decode = do
    _         <- signature
    name      <- first Right Strref.decode
    _         <- first Right $ skipExact 4
    sound     <- first (first (review $ #_SoundError % coercedTo)) Resref.decode
    flags     <- first Right Flags.decode
    sptype    <- first (first (review $ #_SpTypeError % coercedTo)) SpType.decode
    _         <- first Right $ skipExact 1
    exclusion <- first Right Exclusion.decode
    graphics  <- first Right Graphics.decode
    level     <- first (first (review $ #_LevelError % coercedTo)) Level.decode
    school    <- first Right School.decode
    pure undefined
