
{- |
Module: Eeep.Typeclasses.Binary

The binary @Reader@ typeclass.
-}


module Eeep.Typeclasses.Binary (
    -- * Typeclasses.
    Reader (..),
    Writer (..),

    -- * Typeclass functions.
    decode,
    encode,
    patch,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import System.OsPath (OsPath)

-- non-Hackage libraries.
import Trisagion.Parser (Parser, eval)
import Trisagion.Streams.Offset (Offset, initialize)
import Trisagion.Serializer (Serializer, serialize)

-- Package.
import Eeep.IO (readBinary, writeBinary)


{- | Typeclass for types that can be parsed from a (strict) 'ByteString'. -}
class Reader e a where
    {-# MINIMAL parser #-}

    {- | Binary parser for @a@ over streams @s@ with error component @e@. -}
    parser :: Parser (Offset ByteString) e a


{- | Typeclass for types that can both be parsed from and serialized to a (strict) 'ByteString'. -}
class Writer a where
    {-# MINIMAL serializer #-}

    {- | Binary parser for @a@ over streams @s@ with error component @e@. -}
    serializer :: Serializer Builder a


{- | Decode an @a@ from a binary file. -}
decode :: Reader e a => OsPath -> IO (Either e a)
decode path = do
    xs <- initialize <$> readBinary path
    pure $ eval parser xs


{- | Encode an @a@ to a binary file. -}
encode :: Writer a => OsPath -> a -> IO ()
encode path x = writeBinary path (serialize serializer x)


{- | Patch an @a@ in a binary file. -}
patch :: (Reader e a, Writer a) => (a -> a) -> OsPath -> IO (Either e ())
patch patcher path = do
    x <- decode path
    case x of
        Left e  -> pure (Left e) 
        Right y -> Right <$> encode path (patcher y)
