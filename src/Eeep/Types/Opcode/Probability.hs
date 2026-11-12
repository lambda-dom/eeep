{-# LANGUAGE NoFieldSelectors #-}

{- |
Module: Eeep.Types.Opcode.Probability

The @Probability@ type.
-}

module Eeep.Types.Opcode.Probability (
    -- * Error types.
    Error (..),

    -- ** Isomorphisms.
    error,

    -- * Types.
    Probability,

    -- ** Prisms.
    probability,

    -- ** Getters.
    lower,
    upper,

    -- ** Functions.
    isEmpty,
    isElem,
    isContained,
    toList,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Hiding.
import Prelude hiding (error)

-- Base.
import Data.Functor.Contravariant (Contravariant (..))
import Data.Tuple (swap)
import Data.Word (Word8)

-- Libraries.
import Optics.Core (Prism', Iso', prism', review, preview, iso)

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:), validator)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.IStream (InputError)
import Trisagion.Parsers.Combinators (validate)
import qualified Trisagion.Parsers.Combinators as Parsers (pair)
import qualified Trisagion.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Serializer (Serializer)
import qualified Trisagion.Serializers.Combinators as Serializer (pair)
import qualified Trisagion.Serializers.Binary as Serializers (Binary (..))


{- | The t'Probability' @Error@ type.

note(s):

    * To be imported qualified.
-}
data Error = Error {
    lower :: {-# UNPACK #-} !Word8,
    upper :: {-# UNPACK #-} !Word8
} deriving stock (Eq, Ord, Bounded, Show)


{- | Isomorphism for the t'Probability' @Error@ type.

note(s):

    * To be imported qualified.
-}
error :: Iso' Error (Word8, Word8)
error = iso from to
    where
        from :: Error -> (Word8, Word8)
        from (Error m n) = (m, n)

        to :: (Word8, Word8) -> Error
        to = uncurry Error


{- | The @Probability@ interval type.

A /probability interval/ is a closed interval @[l, u]@ with @0 <= l, u <= 100@. If @l > u@ then
the interval is /empty/.
-}
data Probability = Probability !Word8 !Word8
    deriving stock (Eq, Ord, Bounded, Show)


{- | Prism for a t'Probability' interval. -}
{-# INLINABLE probability #-}
probability :: Prism' (Word8, Word8) Probability
probability = prism' construct match
    where
        construct :: Probability -> (Word8, Word8)
        construct (Probability l u) = (l, u)

        match :: (Word8, Word8) -> Maybe Probability
        match (l, u) =
            -- Can have lower > upper in which case interval is empty.
            if l <= 100 && u <= 100 then Just $ Probability l u else Nothing


{- | Return the lower bound of the t'Probability' interval. -}
{-# INLINE lower #-}
lower :: Probability -> Word8
lower = fst . review probability

{- | Return the upper bound of the t'Probability' interval. -}
{-# INLINE upper #-}
upper :: Probability -> Word8
upper = snd . review probability


{- | Return 'True' if the t'Probability' interval is empty. -}
{-# INLINE isEmpty #-}
isEmpty :: Probability -> Bool
isEmpty (Probability l u) = l > u

{- | Return 'True' if @elem@ is an element in the t'Probability' interval. -}
{-# INLINE isElem #-}
isElem :: Word8 -> Probability -> Bool
isElem n (Probability l u) = l <= n && n <= u

{- | Return 'True' if @left@ interval is contained in the right interval. -}
{-# INLINE isContained #-}
isContained :: Probability -> Probability -> Bool
isContained (Probability l1 u1) (Probability l2 u2) = l2 <= l1 && u1 <= u2

{- | Return the list of elements of the t'Probability' interval. -}
{-# INLINE toList #-}
toList :: Probability -> [Word8]
toList (Probability l u) = enumFromTo l u


{- | Decode a t'Probability' interval. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s (InputError :+: Error) Probability
decode =
    validate
        (validator (uncurry Error) (preview probability))
        (fmap swap $ Parsers.pair Parsers.word8 Parsers.word8)

{- | Encode a t'Probability' interval. -}
{-# INLINE encode #-}
encode :: Serializers.Binary s => Serializer s Probability
encode = contramap (review probability) (Serializer.pair Serializers.word8 Serializers.word8)
