{- |
Module: Eeep.Typeclasses.Binary

Typeclasses for binary encoding and decoding.
-}

module Eeep.Typeclasses.Binary (
    -- * Typeclasses.
    Reader (..),
    Writer (..),

    -- * Functions.
    decode,
    encode,
) where

-- Imports.
-- Base.
import Data.Word (Word8)

-- non-Hackage libraries.
import Trisagion.Typeclasses.Source (Source)
import Trisagion.Typeclasses.Sink (Sink)
import Trisagion.Parser (Parser, parse)
import Trisagion.Serializer (Serializer, run)


{- | Typeclass for types that can be parsed from a binary source. -}
class Source Word8 s => Reader s e a where
    {-# MINIMAL parser #-}

    {- | Binary parser for @a@ over input streams @s@ with error component @e@. -}
    parser :: Parser s e a


{- | Typeclass for types that can be serialized to a binary sink. -}
class Sink Word8 b s => Writer b s a where
    {-# MINIMAL serializer #-}

    {- | Binary serializer for @a@ over binary sinks @s@. -}
    serializer :: Serializer s a


{- | Decode an @a@ from a binary source. -}
{-# INLINE decode #-}
decode :: Reader s e a => s -> Either e (a, s)
decode xs = parse parser xs

{- | Encode an @a@ to a binary sink. -}
encode :: Writer b s a => a -> s
{-# INLINE encode #-}
encode x = run serializer x
