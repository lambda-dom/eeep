{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module: Eeep.Typeclasses.Binary

The binary @Reader@ typeclass.
-}


module Eeep.Typeclasses.Binary (
    -- * Typeclasses.
    Reader (..),
    Patcher(..),

    -- * Typeclass functions.
    parseBinary,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import System.OsPath (OsPath)

-- non-Hackage libraries.
import Trisagion.Parser (Parser, eval)
import Trisagion.Streams.Offset (Offset, initialize)
import Trisagion.Serializer (Serializer)

-- Package.
import Eeep.IO (readBinary)


{- | Typeclass for types that can be parsed from a (strict) 'ByteString'. -}
class Reader e a where
    {-# MINIMAL parser #-}

    {- | Binary parser for @a@ over streams @s@ with error component @e@. -}
    parser :: Parser (Offset ByteString) e a


{- | Typeclass for types that can both be parsed from and serialized to a (strict) 'ByteString'. -}
class Reader e a => Patcher e a where
    {-# MINIMAL serializer #-}

    {- | Binary parser for @a@ over streams @s@ with error component @e@. -}
    serializer :: Serializer Builder a


{- | Parse an @a@ from a binary file. -}
parseBinary :: Reader e a => OsPath -> IO (Either e a)
parseBinary path = do
    xs <- initialize <$> readBinary path
    pure $ eval parser xs
