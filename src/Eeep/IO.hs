{- |
Module: Eeep.IO

The @'IO'@ layer.
-}

module Eeep.IO (
    -- * Path formation.
    makePath,

    -- * 'IO' reads.
    readBinary,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import System.OsPath (OsPath, encodeUtf)
import System.File.OsPath (readFile')


{- | Encode a 'String' as an utf8 'OsPath'. -}
makePath :: String -> IO OsPath
makePath = encodeUtf

{- | Read the entire contents of the binary file into memory. -}
readBinary :: OsPath -> IO ByteString
readBinary = readFile'
