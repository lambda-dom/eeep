{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Spell

The @Spell@ type.
-}

module Eeep.Types.Spell (
    -- * Error types.
    Error (..),

    -- * The @t'Spell'@ type.
    Spell (..),

    -- ** Getters.
    abCount,
    opCount,

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
import Data.Vector.Strict (Vector, empty)
import Optics.Core ((^.), (%), (^..), view, folded, toListOf)

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable (..))
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError, matchFoldable, ValidationError (..))
import Trisagion.Parsers.Split (skipExact)
import Trisagion.Typeclasses.Serializers.Binary (char8s)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Combinators (value)
import qualified Trisagion.Serializers.Combinators as Serializers (many)

-- Package.
import Eeep.Types.Shared.Block (Block (..))
import Eeep.Types.Opcode (Opcode)
import qualified Eeep.Types.Opcode as Opcode (Error, encode)
import qualified Eeep.Types.Spell.Ability.Header as AbilityHeader (Header, Error, encode, decode)
import Eeep.Types.Spell.Ability (Ability (..))
import qualified Eeep.Types.Spell.Ability as Ability (opCount)
import qualified Eeep.Types.Spell.Header as SpellHeader (Error, Header, decode, encode)


{- | The t'Spell' @Error@ type. -}
data Error
    = SignatureError !Word8
    | HeaderError    !SpellHeader.Error
    | AbilityError   !AbilityHeader.Error
    | OpcodeError    !Opcode.Error
    deriving stock (Eq, Show, Generic)


{- | The @Spell@ type. -}
data Spell = Spell {
    header    :: !SpellHeader.Header,
    abilities :: !(Vector Ability),
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | Return the number of abilities in a spell. -}
{-# INLINE abCount #-}
abCount :: forall a . Integral a => Spell -> a
abCount = fromIntegral @Int @a . length . view #abilities

-- | Return the offset of the abilities table. Not exported.
{-# INLINE abOffset #-}
abOffset :: forall a . Integral a => Spell -> a
abOffset = const 0x72

-- | Return the offset of the opcodes table. Not exported.
{-# INLINE opOffset #-}
opOffset :: forall a . Integral a => Spell -> a
opOffset spl = abOffset @a spl + (0x28 * abCount @a spl)

{- | Return the number of the global or casting opcodes of the spell. -}
{-# INLINE opCount #-}
opCount :: forall a . Integral a => Spell -> a
opCount = fromIntegral @Int @a . length . view #opcodes


{- | Parser for the @t'Spell'@ signature. -}
{-# INLINE signature #-}
signature
    :: forall b s . (Parsers.Binary b s)
    => Parser s (Error :+: InputError) ()
signature =
    first
        (first (\ (ValidationError n) -> SignatureError n))
        (matchFoldable (fmap (fromIntegral @Int @Word8 . ord) "SPL V1  "))

-- | Parser for the table information of a spell header. Not exported.
{-# INLINEABLE decodeStatic #-}
decodeStatic :: (Parsers.Binary b s) => Parser s InputError (Word16, Word16)
decodeStatic = do
        -- Note: Header offset, opcode table offset and casting opcode offset can all be computed. 
        _         <- skipExact 4
        hdr_count <- Parsers.word16Le
        _         <- skipExact 6
        cst_count <- Parsers.word16Le
        pure (hdr_count, cst_count)


{- | Parser for the @t'Spell'@ type. -}
{-# INLINEABLE decode #-}
decode
    :: (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (Error :+: InputError) (SpellHeader.Header, AbilityHeader.Header, Word16)
decode = do
        _      <- signature
        header <- first (first HeaderError) SpellHeader.decode
        (n, m) <- first Right decodeStatic
        let
            _ = fromIntegral @Word16 @Int n
            _ = fromIntegral @Word16 @Int m
        (hdr, i)   <- first (first AbilityError) AbilityHeader.decode
        pure $ (header, hdr, i)


-- {- | Parser for the @t'Spell'@ type. -}
-- {-# INLINEABLE decode #-}
-- decode :: (Parsers.Binary b s, MonoFoldable Word8 b) => Parser s (Error :+: InputError) Spell
-- decode = do
--         let
--             op_count = cst_count + (sum . fmap snd $ ab_blocks)
--         opcs      <- repeatN op_count $ first (first OpcodeError) Opcode.decode
--         let
--             (cst_opcs, rest_opcs) = splitAt cst_count opcs
--             opcodes               = fromListN cst_count cst_opcs
--             abilities             = fromListN hdr_count $ go rest_opcs ab_blocks
--         pure $ Spell {..}
--     where
--         go :: [Opcode] -> [(Ability.Header, Int)] -> [Ability]
--         go _  []                      = []
--         go xs ((header, !n) : pps) =
--             let
--                 (os, rest) = splitAt n xs
--                 opcodes    = fromListN n os
--             in
--                 Ability {..} : go rest pps

-- | Serializer for the static block of a spell header. Not exported.
{-# INLINE encodeStatic #-}
encodeStatic :: Serializers.Binary b s => Serializer s Spell
encodeStatic
    =  contramap (abOffset) Serializers.word32Le
    <> contramap (abCount) Serializers.word16Le
    <> contramap (opOffset) Serializers.word32Le
    <> contramap (const 0) Serializers.word16Le
    <> contramap (opCount) Serializers.word16Le

{- | Serializer for the t'Spell' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s Spell
encode
        =  value char8s "SPL V1  "
        -- Spell header.
        <> contramap (^. #header) SpellHeader.encode
        -- Static data.
        <> encodeStatic
        -- Headers.
        <> contramap
            (\ spl -> go (opCount spl) (toListOf (#abilities % folded) spl))
            (Serializers.many AbilityHeader.encode)
        -- Opcodes.
        <> contramap (^. #opcodes) (Serializers.many Opcode.encode)
        <> contramap
            (^.. (#abilities % folded % #opcodes))
            (Serializers.many $ Serializers.many Opcode.encode)
    where
        go :: Word16 -> [Ability] -> [(AbilityHeader.Header, Block)]
        go _       []             = []
        go !offset (abil : abils) =
            let
                count = Ability.opCount abil
            in
                (view #header abil, Block {..}) : go (offset + count) abils
