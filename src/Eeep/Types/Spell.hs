{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Spell

The @Spell@ type.
-}

module Eeep.Types.Spell (
    -- * Error types.
    -- * Types.
    Spell (..),

    -- ** Parsers and serializers.
    signature,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Char (ord)
import Data.Word (Word8)

-- Libraries.
import Data.Vector.Strict (Vector)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip, validate)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (takeExact)

-- Package.
import Eeep.Types.Opcode (Opcode)
import Eeep.Types.Spell.Header (Header)
import Eeep.Types.Spell.Ability (Ability)


{- | The t'Spell' @SignatureError@ type.

note(s):

    * To be imported qualified.
-}
newtype SignatureError b = SignatureError b
    deriving stock (Eq, Ord, Show, Generic)


{- | The @Spell@ type. -}
data Spell = Spell {
    header    :: !Header,
    abilities :: !(Vector Ability),
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | Parser for the Spell @t'Header'@ signature. -}
{-# INLINE signature #-}
signature
    :: forall b s . (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (SignatureError b :+: InputError) ()
signature = skip $ validate v (takeExact 8)
    where
        v :: b -> SignatureError b :+: b
        v xs =
            if fmap (fromIntegral @Int @Word8 . ord) "SPL V1  " /= monotoList xs
            then Left $ SignatureError xs
            else Right xs
