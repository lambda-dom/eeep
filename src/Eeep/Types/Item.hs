{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Item

The @Item@ type.
-}

module Eeep.Types.Item (
    -- * Error types.
    -- * Types.
    Item (..),

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
import Data.Void (absurd)
import Data.Word (Word8, Word16)

-- Libraries.
import Data.Vector.Strict (Vector, fromListN)
import Optics.Core (view, (^.), toListOf, (%), folded, (^..))

-- non-Hackage libraries.
import Mono.Typeclasses.MonoFoldable (MonoFoldable)
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError, ValidationError (..), matchFoldable)
import Trisagion.Parsers.Split (skipPrefix)
import Trisagion.Parsers.Combinators (repeatN)
import Trisagion.Typeclasses.Serializers.Binary (char8s)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Serializers.Combinators (value, many)

-- Package.
import Eeep.Types.Shared.Block (Block (..))
import Eeep.Types.Opcode (Opcode)
import qualified Eeep.Types.Opcode as Opcode (Error, decode, encode)
import Eeep.Types.Item.Ability (Ability (..))
import qualified Eeep.Types.Item.Header as ItemHeader (Header, Error, decode, encode)
import qualified Eeep.Types.Item.Ability as Ability (opCount)
import qualified Eeep.Types.Item.Ability.Header as AbilityHeader (Header, Error, decode, encode)


{- | The t'Item' @Error@ type. -}
data Error
    = SignatureError !Word8
    | HeaderError    !ItemHeader.Error
    | AbilityError   !AbilityHeader.Error
    | OpcodeError    !Opcode.Error
    deriving stock (Eq, Show, Generic)


{- | The @Item@ type. -}
data Item = Item {
    header    :: !ItemHeader.Header,
    abilities :: !(Vector Ability),
    opcodes   :: !(Vector Opcode)
} deriving stock (Eq, Show, Generic)


{- | Return the number of abilities in a spell. -}
{-# INLINE abCount #-}
abCount :: forall a . Integral a => Item -> a
abCount = fromIntegral @Int @a . length . view #abilities

-- | Return the offset of the abilities table. Not exported.
{-# INLINE abOffset #-}
abOffset :: forall a . Integral a => Item -> a
abOffset = const 0x72

-- | Return the offset of the opcodes table. Not exported.
{-# INLINE opOffset #-}
opOffset :: forall a . Integral a => Item -> a
opOffset spl = abOffset @a spl + (0x28 * abCount @a spl)

{- | Return the number of the global or casting opcodes of the spell. -}
{-# INLINE opCount #-}
opCount :: forall a . Integral a => Item -> a
opCount = fromIntegral @Int @a . length . view #opcodes


{- | Parser for the @t'Item'@ signature. -}
{-# INLINE signature #-}
signature
    :: forall b s . (Parsers.Binary b s)
    => Parser s (Error :+: InputError) ()
signature =
    first
        (first (\ (ValidationError n) -> SignatureError n))
        (matchFoldable (fmap (fromIntegral @Int @Word8 . ord) "ITM V1  "))


-- | Parser for the table information of a spell header. Not exported.
{-# INLINEABLE decodeStatic #-}
decodeStatic :: (Parsers.Binary b s) => Parser s InputError (Int, Int)
decodeStatic = do
        -- Note: Header offset, opcode table offset and casting opcode offset can all be computed. 
        _        <- first absurd $ skipPrefix 4
        hd_count <- fmap (fromIntegral @Word16 @Int) Parsers.word16Le
        _        <- first absurd $ skipPrefix 6
        cs_count <- fmap (fromIntegral @Word16 @Int) Parsers.word16Le
        pure (hd_count, cs_count)


{- | Parser for the @t'Item'@ type. -}
{-# INLINEABLE decode #-}
decode
    :: (Parsers.Binary b s, MonoFoldable Word8 b)
    => Parser s (Error :+: InputError) Item
decode = do
        _                    <- signature
        header               <- first (first HeaderError) ItemHeader.decode
        (hd_count, cs_count) <- first Right decodeStatic
        abils  <- repeatN hd_count (first (first AbilityError) AbilityHeader.decode)
        let
            op_count = cs_count + (sum . fmap snd $ abils)
        opcs   <- repeatN op_count $ first (first OpcodeError) Opcode.decode
        let
            (cst_opcs, rest_opcs) = splitAt cs_count opcs
            opcodes               = fromListN cs_count cst_opcs
            abilities             = fromListN hd_count $ go rest_opcs abils
        pure $ Item {..}
    where
        go :: [Opcode] -> [(AbilityHeader.Header, Int)] -> [Ability]
        go _  []                      = []
        go xs ((header, !n) : pps) =
            let
                (os, rest) = splitAt n xs
                opcodes    = fromListN n os
            in
                Ability {..} : go rest pps


-- | Serializer for the static block of an item header. Not exported.
{-# INLINE encodeStatic #-}
encodeStatic :: Serializers.Binary b s => Serializer s Item
encodeStatic
    =  contramap (abOffset) Serializers.word32Le
    <> contramap (abCount) Serializers.word16Le
    <> contramap (opOffset) Serializers.word32Le
    <> contramap (const 0) Serializers.word16Le
    <> contramap (opCount) Serializers.word16Le


{- | Serializer for the t'Item' type. -}
{-# INLINEABLE encode #-}
encode :: Serializers.Binary b s => Serializer s Item
encode
        =  value char8s "ITM V1  "
        -- Item header.
        <> contramap (^. #header) ItemHeader.encode
        -- Static data.
        <> encodeStatic
        -- Headers.
        <> contramap
            (\ spl -> go (opCount spl) (toListOf (#abilities % folded) spl))
            (many AbilityHeader.encode)
        -- Opcodes.
        <> contramap (^. #opcodes) (many Opcode.encode)
        <> contramap
            (^.. (#abilities % folded % #opcodes))
            (many $ many Opcode.encode)
    where
        go :: Word16 -> [Ability] -> [(AbilityHeader.Header, Block)]
        go _       []             = []
        go !offset (abil : abils) =
            let
                count = Ability.opCount abil
            in
                (view #header abil, Block {..}) : go (offset + count) abils
