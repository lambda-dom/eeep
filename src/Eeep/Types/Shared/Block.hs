{- |
Module: Eeep.Types.Shared.Block

The @Block@ pair type to keep track of parsed opcode blocks.
-}

module Eeep.Types.Shared.Block (
    -- * Types.
    Block (..),

    -- ** Isomorphisms.
    block,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word16)

-- Libraries.
import Optics.Core (Iso', iso, review, view)

-- non-Hackage libraries.
import Trisagion.Parser (Parser)
import qualified Trisagion.Parsers.Combinators as Parsers (pair)
import Trisagion.Parsers.IStream (InputError)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Combinators as Serializers (pair)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The @Block@ type. -}
data Block = Block {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
    deriving stock (Eq, Ord, Bounded, Show)


{- | Isomorphism for the t'Block' type. -}
block :: Iso' Block (Word16, Word16)
block = iso from to
    where
        from :: Block -> (Word16, Word16)
        from (Block m n) = (m, n)

        to :: (Word16, Word16) -> Block
        to (m, n) = Block m n


{- | Decode a t'Block'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Block
decode = fmap (review block) $ Parsers.pair Parsers.word16Le Parsers.word16Le

{- | Encode a t'Block'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Block
encode = contramap (view block) (Serializers.pair Serializers.word16Le Serializers.word16Le)
