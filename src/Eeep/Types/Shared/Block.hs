{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Eeep.Types.Shared.Block

The pair @Block@ type to keep track of parsed opcode blocks.
-}

module Eeep.Types.Shared.Block (
    -- * Types.
    Block,

    -- ** Isomorphisms.
    block,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word16)

-- Libraries.
import Data.Functor.Contravariant.Divisible (divided)
import Optics.Core (Iso', iso, review, view)

-- non-Hackage libraries.
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (zipP)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)


{- | The @Block@ type. -}
data Block = Block {
    count  :: {-# UNPACK #-} !Word16,
    offset :: {-# UNPACK #-} !Word16
} deriving stock (Eq, Ord, Bounded, Show, Generic)


{- | Isomorphism for the t'Block' type. -}
{-# INLINE block #-}
block :: Iso' Block (Word16, Word16)
block = iso from to
    where
        from :: Block -> (Word16, Word16)
        from (Block m n) = (m, n)

        to :: (Word16, Word16) -> Block
        to (count, offset) = Block {..}


{- | Decode a t'Block'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Block
decode = fmap (review block) $ zipP Parsers.word16Le Parsers.word16Le

{- | Encode a t'Block'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Block
encode = contramap (view block) (divided Serializers.word16Le Serializers.word16Le)
