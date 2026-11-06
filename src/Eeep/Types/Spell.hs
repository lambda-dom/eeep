{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Spell

The @Spell@ type.
-}

module Eeep.Types.Spell (
    -- * Error types.
    Error (..),

    -- * Types.
    Spell (..),

    -- ** Parsers and serializers.
    signature,
    decode,
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
import Trisagion.Parsers.Split (takeExact, skipExact)

-- Package.
import Eeep.Types.Opcode (Opcode)
import Eeep.Types.Spell.Header (Header)
import qualified Eeep.Types.Spell.Header as Header (Error)
import Eeep.Types.Spell.Ability (Ability)
import Data.Bifunctor (Bifunctor(..))


{- | The t'Spell' @Error@ type. -}
data Error b
    = SignatureError !b
    | HeaderError    !Header.Error
    deriving stock (Eq, Show, Generic)


{- | The @Spell@ type. -}
data Spell = Spell {
    header    :: !Header,
    abilities :: !(Vector Ability),
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | Parser for the @t'Spell'@ signature. -}
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


{- | Parser for the @t'Spell'@ type. -}
{-# INLINEABLE decode #-}
decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error b :+: InputError) Spell
decode = do
    _            <- signature
    -- header       <- first (first HeaderError) Header.decode
    -- Note: Header offset, opcode table offset and casting opcode offset can all be computed. 
    _            <- first Right $ skipExact 4
    -- header_count <- first Right $ Parsers.word16Le
    _            <- first Right $ skipExact 6
    -- op_count     <- first Right $ Parsers.word16Le
    pure undefined
