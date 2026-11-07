{- |
Module: Eeep.Types.Shared.Block

The pair @Block@ type to keep track of parsed opcode blocks.
-}

module Eeep.Types.Shared.Block (
    -- * Types.
    Block (..),

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


{- | Decode a t'Block'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Block
decode = fmap (uncurry Block) $ zipP Parsers.word16Le Parsers.word16Le

{- | Encode a t'Block'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Block
encode = contramap (\ (Block m n) -> (m, n)) (divided Serializers.word16Le Serializers.word16Le)
