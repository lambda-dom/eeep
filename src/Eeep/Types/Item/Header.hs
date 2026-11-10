{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Item.Header

The item t'Header' type.
-}

module Eeep.Types.Item.Header (
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
import Data.Word (Word8, Word16, Word32)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), coercedTo, review, preview, view, _1, _2, _3, _4)

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
import Eeep.Types.Shared.Attribute (Attribute)
import qualified Eeep.Types.Shared.Attribute as Attribute (Error (..), decode, encode)
import Eeep.Types.Shared.Percentage (Percentage)
import qualified Eeep.Types.Shared.Percentage as Percentage (Error (..), decode, encode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode, encode)
import Eeep.Types.Shared.Strref (Strref)
import qualified Eeep.Types.Shared.Strref as Strref (decode, encode)
import Eeep.Types.Shared.Proficiency (Proficiency)
import qualified Eeep.Types.Shared.Proficiency as Proficiency (decode, encode)
import Eeep.Types.Shared.Strength (Strength)
import qualified Eeep.Types.Shared.Strength as Strength (strength)
import Eeep.Types.Item.Flags (Flags)
import qualified Eeep.Types.Item.Flags as Flags (decode, encode)
import Eeep.Types.Item.Animation (Animation)
import qualified Eeep.Types.Item.Animation as Animation (decode, encode)
import Eeep.Types.Item.Category (Category)
import qualified Eeep.Types.Item.Category as Category (decode, encode)
import Eeep.Types.Item.KitUsability (KitUsability)
import Eeep.Types.Item.Usability (Usability)
import qualified Eeep.Types.Item.Usability as Usability (decode, encode)
import qualified Eeep.Types.Item.KitUsability as KitUsability


{- | The t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = ItTypeError      {-# UNPACK #-} !Word8
    | StrengthError    {-# UNPACK #-} !Attribute {-# UNPACK #-} !Percentage
    | AttributeError   {-# UNPACK #-} !Word8
    | PercentageError  {-# UNPACK #-} !Word8
    | ReplacementError                !ShortByteString
    | IconError                       !ShortByteString
    deriving stock (Eq, Ord, Generic, Show)


{- | The item @Header@ type. -}
data Header = Header {
    name            :: {-# UNPACK #-} !Strref,
    description     :: {-# UNPACK #-} !Strref,
    nameId          :: {-# UNPACK #-} !Strref,
    descriptionId   :: {-# UNPACK #-} !Strref,
    category        :: {-# UNPACK #-} !Category,
    proficiency     :: {-# UNPACK #-} !Proficiency,
    flags           :: {-# UNPACK #-} !Flags,
    usability       :: {-# UNPACK #-} !Usability,
    kitUsability    :: {-# UNPACK #-} !KitUsability,
    price           :: {-# UNPACK #-} !Word32,
    stack           :: {-# UNPACK #-} !Word16,
    lore            :: {-# UNPACK #-} !Word16,
    weight          :: {-# UNPACK #-} !Word32,
    enchantment     :: {-# UNPACK #-} !Word8,
    level           :: {-# UNPACK #-} !Word8,
    strength        :: {-# UNPACK #-} !Strength,
    intelligence    :: {-# UNPACK #-} !Attribute,
    dexterity       :: {-# UNPACK #-} !Attribute,
    wisdom          :: {-# UNPACK #-} !Attribute,
    constitution    :: {-# UNPACK #-} !Attribute,
    charisma        :: {-# UNPACK #-} !Attribute,
    animation       :: {-# UNPACK #-} !Animation,
    replacement     :: {-# UNPACK #-} !Resref,
    inventoryIcon   :: {-# UNPACK #-} !Resref,
    groundIcon      :: {-# UNPACK #-} !Resref,
    descriptionIcon :: {-# UNPACK #-} !Resref
} deriving stock (Eq, Show, Generic)


-- | Auxiliary 'Strength' parser. Not exported.
{-# INLINE strParser #-}
strParser :: (Parsers.Binary b s) => Parser s (Error :+: InputError) Strength
strParser = do
    strAttr     <- first (first (review $ #_AttributeError % coercedTo)) Attribute.decode
    _           <- first Right $ skipExact 1
    strPerc     <- first (first (review $ #_PercentageError % coercedTo)) Percentage.decode
    let strength = preview Strength.strength (strAttr, strPerc)
    case strength of
        Nothing -> throwError . Left $ StrengthError strAttr strPerc
        Just x  -> pure x

{- | Parser for the item @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Header
decode = do
    name            <- first Right Strref.decode
    nameId          <- first Right Strref.decode
    replacement     <- first (first (review $ #_ReplacementError % coercedTo)) Resref.decode
    flags           <- first Right Flags.decode
    category        <- first Right Category.decode
    usability       <- first Right Usability.decode
    animation       <- first Right Animation.decode
    level           <- first Right Parsers.word8
    _               <- first Right $ skipExact 1
    strength        <- strParser
    kitUse1         <- first Right Parsers.word8
    intelligence    <- first (first (review $ #_AttributeError % coercedTo)) Attribute.decode
    kitUse2         <- first Right Parsers.word8
    dexterity       <- first (first (review $ #_AttributeError % coercedTo)) Attribute.decode
    kitUse3         <- first Right Parsers.word8
    wisdom          <- first (first (review $ #_AttributeError % coercedTo)) Attribute.decode
    kitUse4         <- first Right Parsers.word8
    let kitUsability = review KitUsability.exclusionBytes (kitUse1, kitUse2, kitUse3, kitUse4)
    constitution    <- first (first (review $ #_AttributeError % coercedTo)) Attribute.decode
    proficiency     <- first Right Proficiency.decode
    charisma        <- first (first (review $ #_AttributeError % coercedTo)) Attribute.decode
    _               <- first Right $ skipExact 1
    price           <- first Right Parsers.word32Le
    stack           <- first Right Parsers.word16Le
    inventoryIcon   <- first (first (review $ #_IconError % coercedTo)) Resref.decode
    lore            <- first Right Parsers.word16Le
    groundIcon      <- first (first (review $ #_IconError % coercedTo)) Resref.decode
    weight          <- first Right Parsers.word32Le
    description     <- first Right Strref.decode
    descriptionId   <- first Right Strref.decode
    descriptionIcon <- first (first (review $ #_IconError % coercedTo)) Resref.decode
    enchantment     <- first Right Parsers.word8
    _               <- first Right $ skipExact 3
    pure $ Header {..}

{- | Serializer for the item t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s Header
encode
    =  contramap (^. #name) Strref.encode
    <> contramap (^. #nameId) Strref.encode
    <> contramap (^. #replacement) Resref.encode
    <> contramap (^. #flags) Flags.encode
    <> contramap (^. #category) Category.encode
    <> contramap (^. #usability) Usability.encode
    <> contramap (^. #animation) Animation.encode
    <> contramap (^. #level) Serializers.word8
    <> nulls 1
    <> contramap (fst . review Strength.strength . view #strength) Attribute.encode
    <> nulls 1
    <> contramap (snd . review Strength.strength . view #strength) Percentage.encode
    <> contramap (^. #kitUsability % KitUsability.exclusionBytes % _1) Serializers.word8
    <> contramap (^. #intelligence) Attribute.encode
    <> contramap (^. #kitUsability % KitUsability.exclusionBytes % _2) Serializers.word8
    <> contramap (^. #dexterity) Attribute.encode
    <> contramap (^. #kitUsability % KitUsability.exclusionBytes % _3) Serializers.word8
    <> contramap (^. #wisdom) Attribute.encode
    <> contramap (^. #kitUsability % KitUsability.exclusionBytes % _4) Serializers.word8
    <> contramap (^. #constitution) Attribute.encode
    <> contramap (^. #proficiency) Proficiency.encode
    <> contramap (^. #charisma) Attribute.encode
    <> nulls 1
    <> contramap (^. #price) Serializers.word32Le
    <> contramap (^. #stack) Serializers.word16Le
    <> contramap (^. #inventoryIcon) Resref.encode
    <> contramap (^. #lore) Serializers.word16Le
    <> contramap (^. #groundIcon) Resref.encode
    <> contramap (^. #weight) Serializers.word32Le
    <> contramap (^. #description) Strref.encode
    <> contramap (^. #descriptionId) Strref.encode
    <> contramap (^. #descriptionIcon) Resref.encode
    <> contramap (^. #enchantment) Serializers.word8
    <> nulls 3
