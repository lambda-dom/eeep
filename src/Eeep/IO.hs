{- |
Module: Eeep.IO

The @'IO'@ layer.
-}

module Eeep.IO (
    -- * Path functions.
    fromString,

    -- * Primitive file functions.
    readBinary,
    writeBinary,

    -- * Reading and writing.
    fromFile,
    fromFile',
    toFile,
    patch,
) where

-- Imports.
-- Hiding.
import Prelude hiding (writeFile)

-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, writeFile)
import System.OsPath (OsPath, encodeUtf, decodeUtf)
import System.File.OsPath (readFile')

-- non-Hackage libraries.
import Trisagion.Utils.Either ((:+:))
import Trisagion.IStreams.Offset (Offset, initialize)
import Trisagion.Parser (Parser, eval)
import Trisagion.Serializer (Serializer, run)


{- | Encode a 'String' as an 'OsPath'. -}
fromString :: String -> IO OsPath
fromString = encodeUtf


{- | Read the entire contents of the binary file into memory. -}
readBinary :: OsPath -> IO ByteString
readBinary = readFile'

{- | Write a 'Builder' to a file. -}
writeBinary :: OsPath -> Builder -> IO ()
writeBinary path builder = do
    p <- decodeUtf path
    writeFile p builder


{- | Decode an @a@ from a binary file. -}
fromFile
    :: Parser (Offset ByteString) e a       -- ^ Parser.
    -> OsPath                               -- ^ File path.
    -> IO (e :+: a)
fromFile parser path = do
    xs <- fmap initialize $ readBinary path
    pure $ eval parser xs

{- | Decode an @a@ from a binary file.

The file path is given in the IO monad as a string.
-}
fromFile'
    :: Parser (Offset ByteString) e a       -- ^ Parser.
    -> String                               -- ^ File path.
    -> IO (e :+: a)
fromFile' parser path = fromString path >>= fromFile parser

{- | Encode an @a@ to a binary file. -}
toFile
    :: Serializer Builder a                 -- ^ Serializer.
    -> a                                    -- ^ Value to serialize.
    -> OsPath                               -- ^ File path.
    -> IO ()
toFile s x path = writeBinary path (run s x)

{- | Patch an @a@ in a binary file. -}
patch
    :: Parser (Offset ByteString) e a       -- ^ Parser.
    -> Serializer Builder a                 -- ^ Serializer.
    -> (a -> a)                             -- ^ Patcher.
    -> OsPath                               -- ^ File path.
    -> IO (e :+: ())
patch parser s patcher path = do
    x <- fromFile parser path
    case x of
        Left e  -> pure (Left e) 
        Right y -> Right <$> toFile s (patcher y) path
