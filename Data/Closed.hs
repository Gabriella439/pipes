{-| An empty type that gives cleaner type signatures. -}

module Data.Closed (
    -- * Closed
    C
    ) where

-- | The empty type, denoting a \'@C@\'losed end
data C = C -- Not exported, but I write it to keep the library Haskell98
