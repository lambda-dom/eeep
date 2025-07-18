{- |
Module: Eeep.IO

The @'IO'@ layer.
-}

module Eeep.IO (
    -- * Path formation.
    makePath,

    -- * 'IO' reads.
    readBinary,
    readText,

    -- * 'IO' writes.
    writeBinary,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder (writeFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import System.OsPath (OsPath, encodeUtf, decodeUtf)
import System.File.OsPath (readFile')


{- | Encode a 'String' as an utf-8 'OsPath'. -}
makePath :: String -> IO OsPath
makePath = encodeUtf


{- | Read the entire contents of the binary file into memory. -}
readBinary :: OsPath -> IO ByteString
readBinary = readFile'


{- | Read the entire contents of the utf-8 encoded text file into memory.

Invalid utf-8 characters are replaced with @'\xfffd'@.
-}
readText :: OsPath -> IO Text
readText path = decodeUtf8Lenient <$> readBinary path


{- | Write a 'ByteString' 'Builder' to a file. -}
writeBinary :: OsPath -> Builder -> IO ()
writeBinary path builder = do
    p <- decodeUtf path
    Builder.writeFile p builder
