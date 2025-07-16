{- |
Module: Eeep.IO

The @'IO'@ layer.
-}

module Eeep.IO (
    -- * Path formation.
    makePath,

    -- * 'IO' reads.
    readBinary,

    -- * 'IO' writes.
    writeBinary,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder (writeFile)
import System.OsPath (OsPath, encodeUtf, decodeUtf)
import System.File.OsPath (readFile')


{- | Encode a 'String' as an utf8 'OsPath'. -}
makePath :: String -> IO OsPath
makePath = encodeUtf


{- | Read the entire contents of the binary file into memory. -}
readBinary :: OsPath -> IO ByteString
readBinary = readFile'


{- | Write a 'ByteString' 'Builder' to a file. -}
writeBinary :: OsPath -> Builder -> IO ()
writeBinary path builder = do
    p <- decodeUtf path
    Builder.writeFile p builder
