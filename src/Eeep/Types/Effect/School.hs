{- |
Module: Eeep.Types.Effect.School

The @School@ type.
-}

module Eeep.Types.Effect.School (
    -- * Types.
    School (..),
) where

-- Imports.
-- Base.
import Data.Word (Word32)


{- | The magic t'School' numeric id type. -}
newtype School = School Word32
    deriving stock (Eq, Show)
