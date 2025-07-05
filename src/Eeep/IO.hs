{- |
Module: Eeep.IO

The @'IO'@ layer.
-}

module Eeep.IO (
    -- * 'IO' actions.
    readBinary,

    -- * Parsing.
    parseBinary,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import System.OsPath (OsPath)
import System.File.OsPath (readFile')

-- non-Hackage libraries.
import Trisagion.Streams.Offset (Offset, initialize)
import Trisagion.Parser (Parser, eval)


{- | Read the entire contents of the binary file into memory. -}
readBinary :: OsPath -> IO ByteString
readBinary = readFile'


{- | Read a binary file and parse its contents.

Any unused input by the parser is discarded; if this behavior is undesirable, guard the parser
appropriately.
-}
parseBinary :: Parser (Offset ByteString) e a -> OsPath -> IO (Either e a)
parseBinary parser path = do
    xs <- initialize <$> readBinary path 
    pure $ eval parser xs
