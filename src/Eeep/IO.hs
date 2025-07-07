{- |
Module: Eeep.IO

The @'IO'@ layer.
-}

module Eeep.IO (
    -- * 'IO' actions.
    readBinary,
) where

-- Imports.
-- Libraries.
import Data.ByteString (ByteString)
import System.OsPath (OsPath)
import System.File.OsPath (readFile')


{- | Read the entire contents of the binary file into memory. -}
readBinary :: OsPath -> IO ByteString
readBinary = readFile'


