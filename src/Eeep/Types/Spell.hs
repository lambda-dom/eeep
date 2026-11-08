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
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (ord)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8, Word16)

-- Libraries.
import Data.Vector.Strict (Vector, fromListN, toList)
import Optics.Core ((^.), review, view, folded, (%), toListOf)

-- non-Hackage libraries.
import Data.Word.Optics (word16ToInt)
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (skip, validate, repeatN)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (takeExact, skipExact)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Combinators (applied)
import Trisagion.Typeclasses.Serializers.Binary (char8s)

-- Package.
import Eeep.Types.Shared.Block (Block (..))
import Eeep.Types.Opcode (Opcode)
import qualified Eeep.Types.Opcode as Opcode (Error, decode)
import qualified Eeep.Types.Spell.Header as Spell (Error, Header, decode, encode)
import qualified Eeep.Types.Spell.Ability.Header as Ability (Header, Error, decode)
import qualified Trisagion.Serializers.Combinators as Serializers (repeatN)


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
        hdr_count <- bimap Right (review word16ToInt) $ Parsers.word16Le
        _         <- first Right $ skipExact 6
        cst_count <- bimap Right (review word16ToInt) $ Parsers.word16Le
        ab_blocks <- repeatN hdr_count $ first (first AbilityError) Ability.decode
        let
            op_count = cst_count + (sum . fmap snd $ ab_blocks)
        opcs      <- repeatN op_count $ first (first OpcodeError) Opcode.decode
        let
            (cst_opcs, rest_opcs) = splitAt cst_count opcs
            opcodes               = fromListN cst_count cst_opcs
            abilities             = fromListN hdr_count $ go rest_opcs ab_blocks
        pure $ Spell {..}
    where
        go :: [Opcode] -> [(Ability.Header, Int)] -> [Ability]
        go _  []                      = []
        go xs ((header, !n) : pps) =
            let
                (os, rest) = splitAt n xs
                opcodes    = fromListN n os
            in
                Ability {..} : go rest pps

{- | Serializer for the t'Effect' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s Spell
encode
        =  applied char8s "SPL V1  "
        <> contramap (^. #header) Spell.encode
    where
        lengthWord16 :: Foldable t => t a -> Word16
        lengthWord16 = fromIntegral @Int @Word16 . length

        blocks :: Spell -> [(Block, Ability.Header)]
        blocks spl =
            go
                (lengthWord16 . view #opcodes $ spl)
                (toListOf (#abilities % folded) spl)

        go :: Word16 -> [Ability] -> [(Block, Ability.Header)]
        go _       []             = []
        go !offset (abil : abils) =
            let
                count = lengthWord16 . view #opcodes $ abil
            in
                (Block {..}, view #header abil) : go (count + offset) abils
