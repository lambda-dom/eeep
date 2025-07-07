{- |
Module: Eeep.Typeclasses.Binary

The binary @Reader@ typeclass.
-}


module Eeep.Typeclasses.Binary (
    -- * Typeclasses.
    Reader (..),
) where

-- Imports.
-- non-Hackage libraries.
import Trisagion.Types.ParseError (ParseError)
import Trisagion.Parser (Parser)


class Reader s e a where

    {- | Binary parser for @a@ over streams @s@ with error component @'ParseError' e@. -}
    parser :: Parser s (ParseError e) a
