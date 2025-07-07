{- |
Module: Eeep.Typeclasses.Binary

The binary @Reader@ typeclass.
-}


module Eeep.Typeclasses.Binary (
    -- * Typeclasses.
    Reader (..),

    -- * Typeclass functions.
    parseBinary,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import System.OsPath (OsPath)

-- non-Hackage libraries.
import Trisagion.Parser (Parser, eval)
import Trisagion.Streams.Offset (Offset, initialize)

-- Package.
import Eeep.IO (readBinary)


{- | Typeclass for types that can be parser from (strict) 'ByteString'. -}
class Reader e a where
    {-# MINIMAL parser #-}

    {- | Binary parser for @a@ over streams @s@ with error component @e@. -}
    parser :: Parser (Offset ByteString) e a


{- | Parse an @a@ from a binary file. -}
parseBinary :: Reader e a => OsPath -> IO (Either e a)
parseBinary path = do
    xs <- initialize <$> readBinary path 
    pure $ eval parser xs
