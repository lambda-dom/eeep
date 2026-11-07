{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Spell

The @Spell@ type.
-}

module Eeep.Types.Spell (
    -- * Error types.
    Error (..),

    -- * Types.
    Ability (..),
    Spell (..),

    -- ** Parsers and serializers.
    signature,
    decode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Word (Word8)

-- Libraries.
import Data.Vector.Strict (Vector, fromListN)
import Optics.Core (review)

-- non-Hackage libraries.
import Data.Word.Optics (word16ToWord, word16ToInt)
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip, validate, count)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (takeExact, skipExact)

-- Package.
import qualified Eeep.Types.Shared.Block as Block (count)
import Eeep.Types.Opcode (Opcode)
import qualified Eeep.Types.Opcode as Opcode (Error, decode)
import qualified Eeep.Types.Spell.Header as Spell (Error, Header, decode)
import qualified Eeep.Types.Spell.Ability.Header as Ability (Header, Error, decode)
import Eeep.Types.Shared.Block (Block)


{- | The t'Spell' @Error@ type. -}
data Error b
    = SignatureError !b
    | HeaderError    !Spell.Error
    | AbilityError   !Ability.Error
    | OpcodeError    !Opcode.Error
    deriving stock (Eq, Show, Generic)


{- | The spell @Ability@ type. -}
data Ability = Ability {
    header    :: !Ability.Header,
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | The @Spell@ type. -}
data Spell = Spell {
    header    :: !Spell.Header,
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
        _         <- signature
        header    <- first (first HeaderError) Spell.decode
        -- Note: Header offset, opcode table offset and casting opcode offset can all be computed. 
        _         <- first Right $ skipExact 4
        hdr_count <- first Right $ Parsers.word16Le
        _            <- first Right $ skipExact 6
        cst_count <- first Right $ Parsers.word16Le
        ab_blocks <- count (review word16ToWord hdr_count) $ first (first AbilityError) Ability.decode
        let
            op_count = cst_count + (sum . fmap (Block.count . snd) $ ab_blocks)
        opcs      <- count (review word16ToWord op_count) $ first (first OpcodeError) Opcode.decode
        let
            (cst_opcs, rest_opcs) = splitAt (review word16ToInt cst_count) opcs
            opcodes = fromListN (review word16ToInt cst_count) (cst_opcs)
            abilities = fromListN (review word16ToInt hdr_count) $ go rest_opcs ab_blocks
        pure $ Spell {..}
    where
        go :: [Opcode] -> [(Ability.Header, Block)] -> [Ability]
        go _  []                      = []
        go xs ((header, block) : pps) =
            let
                n          = review word16ToInt $ Block.count block
                (os, rest) = splitAt n xs
                opcodes    = fromListN n os
            in
                Ability {..} : go rest pps
