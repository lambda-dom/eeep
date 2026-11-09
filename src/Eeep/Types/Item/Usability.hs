{- |
Module: Eeep.Types.Item.Usability

The @Usability@ type for the per race/class usability flags field of the main item header.
-}

module Eeep.Types.Item.Usability (
    -- * Types.
    Usability,

    -- ** Isomorphisms.

    -- ** Bit lenses.
    chaotic,

    -- ** Parsers and serializers.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Word (Word32)

-- Libraries.
import Optics.Core (Iso', Lens', (%), coercedTo, view, review)

-- non-Hackage libraries.
import Data.Bits.Optics (bitAt)
import qualified Trisagion.Typeclasses.Parsers.Binary as Parsers (Binary (..))
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Source (InputError)
import qualified Trisagion.Typeclasses.Serializers.Binary as Serializers (Binary (..))
import Trisagion.Serializer (Serializer)
import Data.Functor.Contravariant (Contravariant(..))


{- | The @Usability@ type for the per race/class usability flags field of the main item header.

note(s):

    * The 'Ord' and 'Bounded' instances have no significance and are present merely to be able to
    use t'Usability' values as keys in maps, etc.
-}
newtype Usability = Usability Word32
    deriving stock (Eq, Ord, Bounded, Show)


{- | Isomorphism for the t'Usability' type. -}
{-# INLINE usability #-}
usability :: Iso' Usability Word32
usability = coercedTo


{- | The unsellable t'Usability' bit lens. -}
{-# INLINE chaotic #-}
chaotic :: Lens' Usability Bool
chaotic = usability % bitAt 0


{- | Decode a t'Usability'. -}
{-# INLINE decode #-}
decode :: Parsers.Binary b s => Parser s InputError Usability
decode = fmap (review usability) Parsers.word32Le

{- | Encode a t'Usability'. -}
{-# INLINE encode #-}
encode :: Serializers.Binary b s => Serializer s Usability
encode = contramap (view usability) Serializers.word32Le
