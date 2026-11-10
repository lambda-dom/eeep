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
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Int (Int16)
import Data.Word (Word8, Word16)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Data.ByteString.Short (ShortByteString)
import Optics.Core ((%), review, coercedTo)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..), int16Le)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (skipExact)

-- Package.
import Eeep.Types.Shared.Ability.Location (Location)
import qualified Eeep.Types.Shared.Ability.Location as Location (Error (..), decode)
import Eeep.Types.Shared.Ability.Range (Range)
import qualified Eeep.Types.Shared.Ability.Range as Range (decode)
import Eeep.Types.Shared.Ability.Speed (Speed)
import qualified Eeep.Types.Shared.Ability.Speed as Speed (Error (..), decode)
import Eeep.Types.Shared.Ability.Target (Target)
import qualified Eeep.Types.Shared.Ability.Target as Target (Error (..), decode)
import Eeep.Types.Shared.Dice (Dice)
import qualified Eeep.Types.Shared.Dice as Dice (dice)
import Eeep.Types.Shared.Projectile (Projectile)
import qualified Eeep.Types.Shared.Projectile as Projectile (decode)
import Eeep.Types.Shared.Resref (Resref)
import qualified Eeep.Types.Shared.Resref as Resref (Error (..), decode)
import Eeep.Types.Shared.School (School)
import qualified Eeep.Types.Shared.School as School (decode)
import Eeep.Types.Shared.Sectype (Sectype)
import qualified Eeep.Types.Shared.Sectype as Sectype (decode)
import Eeep.Types.Item.Ability.Attack (Attack)
import qualified Eeep.Types.Item.Ability.Attack as Attack (Error (..), decode)
import Eeep.Types.Item.Ability.Charge (Charge)
import qualified Eeep.Types.Item.Ability.Charge as Charge (Error (..), decode)
import Eeep.Types.Item.Ability.Damage (Damage)
import qualified Eeep.Types.Item.Ability.Damage as Damage (Error (..), decode)
import Eeep.Types.Item.Ability.Flags (Flags)
import qualified Eeep.Types.Item.Ability.Flags as Flags (decode)
import Eeep.Types.Item.Ability.Launcher (Launcher)
import qualified Eeep.Types.Item.Ability.Launcher as Launcher (Error (..), decode)
import Eeep.Types.Item.Ability.Melee (Melee)
import qualified Eeep.Types.Item.Ability.Melee as Melee (Error, decode)


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
idRequiredParser = do
    n <- first Right Parsers.word8
    case n of
        1 -> pure True
        2 -> pure False
        _ -> throwError . Left $ IdRequiredError n

-- | isParser aux parser. Not exported.
{-# INLINE isParser #-}
isParser :: (Parsers.Binary b s) => Parser s (Error :+: InputError) Bool
isParser = do
    n <- first Right Parsers.word8
    _ <- first Right $ skipExact 1
    case n of
        0 -> pure False
        1 -> pure True
        _ -> throwError . Left $ IsAmmoError n


{- | Parser for the item @t'Header'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) (Header, Int)
decode = do
    attack     <- first (first (review $ #_AttackError % coercedTo)) Attack.decode
    idRequired <- idRequiredParser
    location   <- first (first (review $ #_LocationError % coercedTo)) Location.decode
    _          <- first Right $ skipExact 1
    icon       <- first (first (review $ #_IconError % coercedTo)) Resref.decode
    target     <- first (first (review $ #_TargetError % coercedTo)) Target.decode
    count      <- first Right Parsers.word8
    range      <- first Right Range.decode
    launcher   <- first (first (review $ #_LauncherError % coercedTo)) Launcher.decode
    _          <- first Right $ skipExact 1
    speed      <- first (first (review $ #_SpeedError % coercedTo)) Speed.decode
    _          <- first Right $ skipExact 1
    thac0      <- first Right Parsers.int16Le
    sides      <- first Right Parsers.word8
    school     <- first Right School.decode
    number     <- first Right Parsers.word8
    let dice = review Dice.dice (sides, number)
    sectype    <- first Right Sectype.decode
    damage     <- first Right Parsers.int16Le
    damtype    <- first (first (review $ #_DamageError % coercedTo)) Damage.decode
    _          <- first Right $ skipExact 1
    op_count   <- bimap Right (fromIntegral @Word16 @Int) Parsers.word16Le
    _          <- first Right $ skipExact 2
    charges    <- first Right Parsers.word16Le
    charge     <- first (first (review $ #_ChargeError % coercedTo)) Charge.decode
    _          <- first Right $ skipExact 1
    flags      <- first Right Flags.decode
    projectile <- first Right Projectile.decode
    melee      <- first (first MeleeError) Melee.decode
    isArrow    <- isParser
    isBolt     <- isParser
    isBullet   <- isParser
    pure $ (Header {..}, op_count)
