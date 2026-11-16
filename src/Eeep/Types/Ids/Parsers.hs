{- |
Module: Eeep.Types.Ids.Parsers

Auxiliary types and parsers for table formats.
-}

module Eeep.Types.Ids.Parsers (
    -- * Type aliases.
    Line,
    Lines (..),

    -- * Parsers.
    signature,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

-- non-Hackage libraries.
import Data.MonoFunctor (MonoFunctor)
import Data.MonoPointed (MonoPointed)
import Trisagion.Utils.Either ((:+:))
import Trisagion.IStream (IStream)
import Trisagion.IStreams.Offset (Offset)
import Trisagion.IStreams.Counter (Counter)
import Trisagion.IStreams.Char (Chars)
import Trisagion.Parser (Parser)
import Trisagion.Parsers.Combinators (validate)
import Trisagion.Parsers.IStream (InputError, ValidationError (..), one)


{- | A type alias for @t'Offset' 'ByteString'@. -}
type Line = Offset (Chars ByteString)

{- | A type wrapper for @t'Counter' ['ByteString']@. -}
newtype Lines = Lines (Counter [ByteString])
    deriving stock Eq
    deriving newtype (Show, MonoFunctor ByteString, MonoPointed ByteString, IStream ByteString)


{- | Parser for the signature. -}
signature :: Parser Lines (InputError :+: ValidationError ByteString) ()
signature = validate v one
    where
        v :: ByteString -> ValidationError ByteString :+: ()
        v xs = if xs == pack "IDS v1.0" then Right () else Left $ ValidationError xs
