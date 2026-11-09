{- |
Module: Eeep.Types.Item.Ability.Melee

The pair @Melee@ type to keep track of parsed opcode blocks.
-}

module Eeep.Types.Item.Ability.Melee (
    -- * Error types.
    Error (..),

    -- * Types.
    Melee (..),

    -- ** Prisms.
    melee,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import GHC.Generics (Generic)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.Word (Word8)

-- Libraries.
import Control.Monad.Except (MonadError (..))
import Optics.Core (Prism', (^.), prism', preview)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import Trisagion.Parsers.Split (skipExact)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Trisagion.Typeclasses.Serializers.Binary (nulls)


{- | The t'Melee' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error = Error {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving stock (Eq, Ord, Bounded, Show, Generic)


{- | The @Melee@ animation frequency type. -}
data Melee = Melee {
    overhand :: {-# UNPACK #-} !Word8,
    backhand :: {-# UNPACK #-} !Word8,
    thrust   :: {-# UNPACK #-} !Word8
} deriving stock (Eq, Ord, Bounded, Generic, Show)


{- | Decode a t'Melee'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (Error :+: InputError) Melee
decode = do
    m <- first Right Parsers.word8
    _ <- first Right $ skipExact 1
    n <- first Right Parsers.word8
    _ <- first Right $ skipExact 1
    p <- first Right Parsers.word8
    _ <- first Right $ skipExact 1
    case preview melee (m, n, p) of
        Nothing -> throwError . Left $ Error m n p
        Just x  -> pure x

{- | Prism for the t'Melee' type. -}
{-# INLINE melee #-}
melee :: Prism' (Word8, Word8, Word8) Melee
melee = prism' construct match
    where
        construct :: Melee -> (Word8, Word8, Word8)
        construct (Melee m n p) = (m, n, p)

        match :: (Word8, Word8, Word8) -> Maybe Melee
        match (m, n, p) =
            if m <= 100 && n <= 100 && p <= 100 && (m + n + p == 100 || (m == 0 && n == 0 && p == 0))
            then Just $ Melee m n p
            else Nothing

{- | Encode a t'Melee'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Melee
encode
    =  contramap (^. #overhand) Serializers.word8
    <> nulls 1
    <> contramap (^. #backhand) Serializers.word8
    <> nulls 1
    <> contramap (^. #thrust) Serializers.word8
    <> nulls 1
