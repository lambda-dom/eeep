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
import Optics.Core ((%), (^.), review, coercedTo, _1, _2)

-- non-Hackage libraries.
import Data.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parsers.Split (skipExact, skipN)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..), int16Le)
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Binary (nullN)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..), int16Le)

-- Package.
import Eeep.Types.Shared.Ability.Location (Location)
import qualified Eeep.Types.Shared.Ability.Location as Location (Error (..), decode, encode)
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
import qualified Eeep.Types.Shared.Resref as Resref (decode, encode)
import Eeep.Types.Item.Ability.AttackType (AttackType)
import qualified Eeep.Types.Item.Ability.AttackType as AttackType (Error (..), decode, encode)
import Eeep.Types.Item.Ability.Charge (Charge)
import qualified Eeep.Types.Item.Ability.Charge as Charge (Error (..), decode, encode)
import Eeep.Types.Item.Ability.DamageType (DamageType)
import qualified Eeep.Types.Item.Ability.DamageType as DamageType (Error (..), decode, encode)
import Eeep.Types.Item.Ability.Flags (Flags)
import qualified Eeep.Types.Item.Ability.Flags as Flags (decode, encode)
import Eeep.Types.Item.Ability.Launcher (Launcher)
import qualified Eeep.Types.Item.Ability.Launcher as Launcher (Error (..), decode, encode)
import Eeep.Types.Item.Ability.Melee (Melee)
import qualified Eeep.Types.Item.Ability.Melee as Melee (Error, decode, encode)


{- | The t'Header' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error
    = AttackTypeError {-# UNPACK #-} !Word8
    | IdRequiredError {-# UNPACK #-} !Word8
    | LocationError   {-# UNPACK #-} !Word8
    | TargetError     {-# UNPACK #-} !Word8
    | LauncherError   {-# UNPACK #-} !Word8
    | SpeedError      {-# UNPACK #-} !Word8
    | DamageTypeError {-# UNPACK #-} !Word8
    | ChargeError     {-# UNPACK #-} !Word8
    | IsAmmoError     {-# UNPACK #-} !Word8
    | MeleeError                     !Melee.Error
    deriving stock (Eq, Ord, Generic, Show)

{- | The item ability @Header@ type. -}
data Header = Header {
    attacktype  :: {-# UNPACK #-} !AttackType,
    idRequired  :: {-# UNPACK #-} !Bool,
    location    :: {-# UNPACK #-} !Location,
    icon        :: {-# UNPACK #-} !Resref,
    target      :: {-# UNPACK #-} !Target,
    count       :: {-# UNPACK #-} !Word8,
    range       :: {-# UNPACK #-} !Word16,
    launcher    :: {-# UNPACK #-} !Launcher,
    speed       :: {-# UNPACK #-} !Speed,
    thac0       :: {-# UNPACK #-} !Int16,
    damage      :: {-# UNPACK #-} !Int16,
    dice        :: {-# UNPACK #-} !Dice,
    damtype     :: {-# UNPACK #-} !DamageType,
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
idRequiredParser :: (Parsers.Binary b s) => Parser s (InputError :+: Error) Bool
idRequiredParser = validate v Parsers.word8
    where
        v :: Word8 -> Error :+: Bool
        v 0 = Right True
        v 1 = Right False
        v n = Left $ IdRequiredError n

-- | idRequired aux serializer. Not exported.
{-# INLINE idRequiredSerializer #-}
idRequiredSerializer :: Serializers.Binary s => Serializer s Bool
idRequiredSerializer = contramap (fromIntegral @Int @Word8 . fromEnum . not) Serializers.word8
        

-- | isParser aux parser. Not exported.
{-# INLINE isParser #-}
isParser :: (Parsers.Binary b s) => Parser s (InputError :+: Error) Bool
isParser = validate v Parsers.word8
    where
        v :: Word8 -> Error :+: Bool
        v 0 = Right False
        v 1 = Right True
        v n = Left $ IsAmmoError n

-- | isSerializer aux serializer. Not exported.
{-# INLINE isSerializer #-}
isSerializer :: Serializers.Binary s => Serializer s Bool
isSerializer = contramap (fromIntegral @Int @Word8 . fromEnum) Serializers.word8


{- | Parser for the item @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (InputError :+: Error) (Header, Int)
decode = do
    attacktype <- first (fmap (review $ #_AttackTypeError % coercedTo)) AttackType.decode
    idRequired <- idRequiredParser
    location   <- first (fmap (review $ #_LocationError % coercedTo)) Location.decode
    _          <- first absurd $ skipN 1
    icon       <- first Left Resref.decode
    target     <- first (fmap (review $ #_TargetError % coercedTo)) Target.decode
    count      <- first Left Parsers.word8
    range      <- first Left Parsers.word16Le
    launcher   <- first (fmap (review $ #_LauncherError % coercedTo)) Launcher.decode
    _          <- first absurd $ skipN 1
    speed      <- first (fmap (review $ #_SpeedError % coercedTo)) Speed.decode
    _          <- first absurd $ skipN 1
    thac0      <- first Left Parsers.int16Le
    sides      <- first Left Parsers.word8
    _          <- first absurd $ skipN 1
    number     <- first Left Parsers.word8
    let dice = review Dice.dice (number, sides)
    _          <- first absurd $ skipN 1
    damage     <- first Left Parsers.int16Le
    damtype    <- first (fmap (review $ #_DamageTypeError % coercedTo)) DamageType.decode
    _          <- first absurd $ skipN 1
    op_count   <- bimap Left (fromIntegral @Word16 @Int) Parsers.word16Le
    -- Note: opcode offset not needed.
    _          <- first absurd $ skipN 2
    charges    <- first Left Parsers.word16Le
    charge     <- first (fmap (review $ #_ChargeError % coercedTo)) Charge.decode
    _          <- first absurd $ skipN 1
    flags      <- first Left Flags.decode
    projectile <- first Left Projectile.decode
    melee      <- first (fmap MeleeError) Melee.decode
    isArrow    <- isParser
    _          <- first absurd $ skipN 1
    isBolt     <- isParser
    _          <- first absurd $ skipN 1
    isBullet   <- isParser
    _ <- first Left $ skipExact 1
    pure $ (Header {..}, op_count)


{- | Serializer for the spell ability t'Header' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary s => Serializer s (Header, Block)
encode
    =  contramap (^. _1 % #attacktype) AttackType.encode
    <> contramap (^. _1 % #idRequired) idRequiredSerializer
    <> contramap (^. _1 % #location) Location.encode
    <> nullN 1
    <> contramap (^. _1 % #icon) Resref.encode
    <> contramap (^. _1 % #target) Target.encode
    <> contramap (^. _1 % #count) Serializers.word8
    <> contramap (^. _1 % #range) Serializers.word16Le
    <> contramap (^. _1 % #launcher) Launcher.encode
    <> nullN 1
    <> contramap (^. _1 % #speed) Speed.encode
    <> nullN 1
    <> contramap (^. _1 % #thac0) Serializers.int16Le
    <> contramap (^. _1 % #dice % Dice.dice % _2) Serializers.word8
    <> nullN 1
    <> contramap (^. _1 % #dice % Dice.dice % _1) Serializers.word8
    <> nullN 1
    <> contramap (^. _1 % #damage) Serializers.int16Le
    <> contramap (^. _1 % #damtype) DamageType.encode
    <> nullN 1
    <> contramap (^. _2) Block.encode
    <> contramap (^. _1 % #charges) Serializers.word16Le
    <> contramap (^. _1 % #charge) Charge.encode
    <> nullN 1
    <> contramap (^. _1 % #flags) Flags.encode
    <> contramap (^. _1 % #projectile) Projectile.encode
    <> contramap (^. _1 % #melee) Melee.encode
    <> contramap (^. _1 % #isArrow) isSerializer
    <> nullN 1
    <> contramap (^. _1 % #isBolt) isSerializer
    <> nullN 1
    <> contramap (^. _1 % #isBullet) isSerializer
    <> nullN 1
