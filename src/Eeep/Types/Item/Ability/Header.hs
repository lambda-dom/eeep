{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Item.Ability.Header

The item ability @Header@ type.
-}

module Eeep.Types.Item.Ability.Header (
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
import Data.Int (Int16)
import Data.Void (absurd)
import Data.Word (Word8, Word16)

-- Libraries.
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), (^.), review, coercedTo, _1, _2)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..), int16Le)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (skipExact, skipPrefix)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..), int16Le)
import Trisagion.Serializer (Serializer)

-- Package.
import Eeep.Types.Shared.Ability.Location (Location)
import qualified Eeep.Types.Shared.Ability.Location as Location (Error (..), decode, encode)
import Eeep.Types.Shared.Ability.Range (Range)
import qualified Eeep.Types.Shared.Ability.Range as Range (decode, encode)
import Eeep.Types.Shared.Ability.Speed (Speed)
import qualified Eeep.Types.Shared.Ability.Speed as Speed (Error (..), decode, encode)
import Eeep.Types.Shared.Ability.Target (Target)
import qualified Eeep.Types.Shared.Ability.Target as Target (Error (..), decode, encode)
import Eeep.Types.Shared.Block (Block)
import qualified Eeep.Types.Shared.Block as Block (encode)
import Eeep.Types.Shared.Dice (Dice)
import qualified Eeep.Types.Shared.Dice as Dice (dice)
import Eeep.Types.Shared.Projectile (Projectile)
import qualified Eeep.Types.Shared.Projectile as Projectile (decode, encode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode, encode)
import Eeep.Types.Shared.School (School)
import qualified Eeep.Types.Shared.School as School (decode, encode)
import Eeep.Types.Shared.Sectype (Sectype)
import qualified Eeep.Types.Shared.Sectype as Sectype (decode, encode)
import Eeep.Types.Item.Ability.Attack (Attack)
import qualified Eeep.Types.Item.Ability.Attack as Attack (Error (..), decode, encode)
import Eeep.Types.Item.Ability.Charge (Charge)
import qualified Eeep.Types.Item.Ability.Charge as Charge (Error (..), decode, encode)
import Eeep.Types.Item.Ability.Damage (Damage)
import qualified Eeep.Types.Item.Ability.Damage as Damage (Error (..), decode, encode)
import Eeep.Types.Item.Ability.Flags (Flags)
import qualified Eeep.Types.Item.Ability.Flags as Flags (decode, encode)
import Eeep.Types.Item.Ability.Launcher (Launcher)
import qualified Eeep.Types.Item.Ability.Launcher as Launcher (Error (..), decode, encode)
import Eeep.Types.Item.Ability.Melee (Melee)
import qualified Eeep.Types.Item.Ability.Melee as Melee (Error, decode, encode)
import Trisagion.Typeclasses.Serializers.Binary (nulls)


{- | The t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = AttackError     {-# UNPACK #-} !Word8
    | IdRequiredError {-# UNPACK #-} !Word8
    | LocationError   {-# UNPACK #-} !Word8
    | TargetError     {-# UNPACK #-} !Word8
    | LauncherError   {-# UNPACK #-} !Word8
    | SpeedError      {-# UNPACK #-} !Word8
    | DamageError     {-# UNPACK #-} !Word8
    | ChargeError     {-# UNPACK #-} !Word8
    | IsAmmoError     {-# UNPACK #-} !Word8
    | MeleeError                     !Melee.Error
    | IconError                      !ShortByteString
    deriving stock (Eq, Ord, Generic, Show)

{- | The item ability @Header@ type. -}
data Header = Header {
    attack      :: {-# UNPACK #-} !Attack,
    idRequired  :: {-# UNPACK #-} !Bool,
    location    :: {-# UNPACK #-} !Location,
    icon        :: {-# UNPACK #-} !Resref,
    target      :: {-# UNPACK #-} !Target,
    count       :: {-# UNPACK #-} !Word8,
    range       :: {-# UNPACK #-} !Range,
    launcher    :: {-# UNPACK #-} !Launcher,
    speed       :: {-# UNPACK #-} !Speed,
    thac0       :: {-# UNPACK #-} !Int16,
    damage      :: {-# UNPACK #-} !Int16,
    dice        :: {-# UNPACK #-} !Dice,
    school      :: {-# UNPACK #-} !School,
    sectype     :: {-# UNPACK #-} !Sectype,
    damtype     :: {-# UNPACK #-} !Damage,
    charge      :: {-# UNPACK #-} !Charge,
    charges     :: {-# UNPACK #-} !Word16,
    flags       :: {-# UNPACK #-} !Flags,
    projectile  :: {-# UNPACK #-} !Projectile,
    melee       :: {-# UNPACK #-} !Melee,
    isArrow     :: {-# UNPACK #-} !Bool,
    isBolt      :: {-# UNPACK #-} !Bool,
    isBullet    :: {-# UNPACK #-} !Bool
} deriving stock (Eq, Show, Generic)


-- | idRequired aux parser. Not exported.
{-# INLINE idRequiredParser #-}
idRequiredParser :: (Parsers.Binary b s) => Parser s (Error :+: InputError) Bool
idRequiredParser = validate v Parsers.word8
    where
        v :: Word8 -> Error :+: Bool
        v 0 = Right True
        v 1 = Right False
        v n = Left $ IdRequiredError n

-- | idRequired aux serializer. Not exported.
{-# INLINE idRequiredSerializer #-}
idRequiredSerializer :: Serializers.Binary b s => Serializer s Bool
idRequiredSerializer = contramap (fromIntegral @Int @Word8 . fromEnum . not) Serializers.word8
        

-- | isParser aux parser. Not exported.
{-# INLINE isParser #-}
isParser :: (Parsers.Binary b s) => Parser s (Error :+: InputError) Bool
isParser = validate v Parsers.word8
    where
        v :: Word8 -> Error :+: Bool
        v 0 = Right False
        v 1 = Right True
        v n = Left $ IsAmmoError n

-- | isSerializer aux serializer. Not exported.
{-# INLINE isSerializer #-}
isSerializer :: Serializers.Binary b s => Serializer s Bool
isSerializer = contramap (fromIntegral @Int @Word8 . fromEnum) Serializers.word8


{- | Parser for the item @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) (Header, Int)
decode = do
    attack     <- first (first (review $ #_AttackError % coercedTo)) Attack.decode
    idRequired <- idRequiredParser
    location   <- first (first (review $ #_LocationError % coercedTo)) Location.decode
    _          <- first absurd $ skipPrefix 1
    icon       <- first (first (review $ #_IconError % coercedTo)) Resref.decode
    target     <- first (first (review $ #_TargetError % coercedTo)) Target.decode
    count      <- first Right Parsers.word8
    range      <- first Right Range.decode
    launcher   <- first (first (review $ #_LauncherError % coercedTo)) Launcher.decode
    _          <- first absurd $ skipPrefix 1
    speed      <- first (first (review $ #_SpeedError % coercedTo)) Speed.decode
    _          <- first absurd $ skipPrefix 1
    thac0      <- first Right Parsers.int16Le
    sides      <- first Right Parsers.word8
    school     <- first Right School.decode
    number     <- first Right Parsers.word8
    let dice = review Dice.dice (number, sides)
    sectype    <- first Right Sectype.decode
    damage     <- first Right Parsers.int16Le
    damtype    <- first (first (review $ #_DamageError % coercedTo)) Damage.decode
    _          <- first absurd $ skipPrefix 1
    op_count   <- bimap Right (fromIntegral @Word16 @Int) Parsers.word16Le
    -- Note: opcode offset not needed.
    _          <- first absurd $ skipPrefix 2
    charges    <- first Right Parsers.word16Le
    charge     <- first (first (review $ #_ChargeError % coercedTo)) Charge.decode
    _          <- first absurd $ skipPrefix 1
    flags      <- first Right Flags.decode
    projectile <- first Right Projectile.decode
    melee      <- first (first MeleeError) Melee.decode
    isArrow    <- isParser
    _          <- first absurd $ skipPrefix 1
    isBolt     <- isParser
    _          <- first absurd $ skipPrefix 1
    isBullet   <- isParser
    _ <- first Right $ skipExact 1
    pure $ (Header {..}, op_count)


{- | Serializer for the spell ability t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s (Header, Block)
encode
    =  contramap (^. _1 % #attack) Attack.encode
    <> contramap (^. _1 % #idRequired) idRequiredSerializer
    <> contramap (^. _1 % #location) Location.encode
    <> nulls 1
    <> contramap (^. _1 % #icon) Resref.encode
    <> contramap (^. _1 % #target) Target.encode
    <> contramap (^. _1 % #count) Serializers.word8
    <> contramap (^. _1 % #range) Range.encode
    <> contramap (^. _1 % #launcher) Launcher.encode
    <> nulls 1
    <> contramap (^. _1 % #speed) Speed.encode
    <> nulls 1
    <> contramap (^. _1 % #thac0) Serializers.int16Le
    <> contramap (^. _1 % #dice % Dice.dice % _2) Serializers.word8
    <> contramap (^. _1 % #school) School.encode
    <> contramap (^. _1 % #dice % Dice.dice % _1) Serializers.word8
    <> contramap (^. _1 % #sectype) Sectype.encode
    <> contramap (^. _1 % #damage) Serializers.int16Le
    <> contramap (^. _1 % #damtype) Damage.encode
    <> nulls 1
    <> contramap (^. _2) Block.encode
    <> contramap (^. _1 % #charges) Serializers.word16Le
    <> contramap (^. _1 % #charge) Charge.encode
    <> nulls 1
    <> contramap (^. _1 % #flags) Flags.encode
    <> contramap (^. _1 % #projectile) Projectile.encode
    <> contramap (^. _1 % #melee) Melee.encode
    <> contramap (^. _1 % #isArrow) isSerializer
    <> nulls 1
    <> contramap (^. _1 % #isBolt) isSerializer
    <> nulls 1
    <> contramap (^. _1 % #isBullet) isSerializer
    <> nulls 1
