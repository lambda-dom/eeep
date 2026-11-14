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
import Data.Void (absurd)
import Data.Word (Word8, Word16, Word32)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), coercedTo, review, _1, _2, _3, _4)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parsers.Split (skipExact, skipN)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (nullN)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))

-- Package.
import Eeep.Types.Shared.Attribute (Attribute)
import qualified Eeep.Types.Shared.Attribute as Attribute (Error (..), decode, encode)
import Eeep.Types.Shared.Percentage (Percentage)
import qualified Eeep.Types.Shared.Percentage as Percentage (Error (..), decode, encode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (decode, encode)
import Eeep.Types.Shared.Strref (Strref)
import qualified Eeep.Types.Shared.Strref as Strref (decode, encode)
import Eeep.Types.Shared.Proficiency (Proficiency)
import qualified Eeep.Types.Shared.Proficiency as Proficiency (decode, encode)
import Eeep.Types.Item.Flags (Flags)
import qualified Eeep.Types.Item.Flags as Flags (decode, encode)
import Eeep.Types.Item.Animation (Animation)
import qualified Eeep.Types.Item.Animation as Animation (decode, encode)
import Eeep.Types.Item.Category (Category)
import qualified Eeep.Types.Item.Category as Category (decode, encode)
import Eeep.Types.Item.Usability (Usability)
import qualified Eeep.Types.Item.Usability as Usability (decode, encode)
import Eeep.Types.Item.KitUsability (KitUsability, exclusionBytes)


{- | The t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = ItemTypeError    {-# UNPACK #-} !Word8
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
    strength        :: {-# UNPACK #-} !Attribute,
    strengthBonus   :: {-# UNPACK #-} !Percentage,
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


{- | Parser for the item @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (InputError :+: Error) Header
decode = do
    name            <- first Left Strref.decode
    nameId          <- first Left Strref.decode
    replacement     <- first Left Resref.decode
    flags           <- first Left Flags.decode
    category        <- first Left Category.decode
    usability       <- first Left Usability.decode
    animation       <- first Left Animation.decode
    level           <- first Left Parsers.word8
    _               <- first absurd $ skipN 1
    strength        <- first (fmap (review $ #_AttributeError % coercedTo)) Attribute.decode
    _               <- first absurd $ skipN 1
    strengthBonus   <- first (fmap (review $ #_PercentageError % coercedTo)) Percentage.decode
    kitUse1         <- first Left Parsers.word8
    intelligence    <- first (fmap (review $ #_AttributeError % coercedTo)) Attribute.decode
    kitUse2         <- first Left Parsers.word8
    dexterity       <- first (fmap (review $ #_AttributeError % coercedTo)) Attribute.decode
    kitUse3         <- first Left Parsers.word8
    wisdom          <- first (fmap (review $ #_AttributeError % coercedTo)) Attribute.decode
    kitUse4         <- first Left Parsers.word8
    let kitUsability = review exclusionBytes (kitUse1, kitUse2, kitUse3, kitUse4)
    constitution    <- first (fmap (review $ #_AttributeError % coercedTo)) Attribute.decode
    proficiency     <- first Left Proficiency.decode
    charisma        <- first (fmap (review $ #_AttributeError % coercedTo)) Attribute.decode
    _               <- first absurd $ skipN 1
    price           <- first Left Parsers.word32Le
    stack           <- first Left Parsers.word16Le
    inventoryIcon   <- first Left Resref.decode
    lore            <- first Left Parsers.word16Le
    groundIcon      <- first Left Resref.decode
    weight          <- first Left Parsers.word32Le
    description     <- first Left Strref.decode
    descriptionId   <- first Left Strref.decode
    descriptionIcon <- first Left Resref.decode
    enchantment     <- first Left Parsers.word8
    _               <- first Left $ skipExact 3
    pure $ Header {..}

{- | Serializer for the item t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary s => Serializer s Header
encode
    =  contramap (^. #name) Strref.encode
    <> contramap (^. #nameId) Strref.encode
    <> contramap (^. #replacement) Resref.encode
    <> contramap (^. #flags) Flags.encode
    <> contramap (^. #category) Category.encode
    <> contramap (^. #usability) Usability.encode
    <> contramap (^. #animation) Animation.encode
    <> contramap (^. #level) Serializers.word8
    <> nullN 1
    <> contramap (^. #strength) Attribute.encode
    <> nullN 1
    <> contramap (^. #strengthBonus) Percentage.encode
    <> contramap (^. #kitUsability % exclusionBytes % _1) Serializers.word8
    <> contramap (^. #intelligence) Attribute.encode
    <> contramap (^. #kitUsability % exclusionBytes % _2) Serializers.word8
    <> contramap (^. #dexterity) Attribute.encode
    <> contramap (^. #kitUsability % exclusionBytes % _3) Serializers.word8
    <> contramap (^. #wisdom) Attribute.encode
    <> contramap (^. #kitUsability % exclusionBytes % _4) Serializers.word8
    <> contramap (^. #constitution) Attribute.encode
    <> contramap (^. #proficiency) Proficiency.encode
    <> contramap (^. #charisma) Attribute.encode
    <> nullN 1
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
    <> nullN 3
