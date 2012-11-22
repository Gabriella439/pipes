{-| An empty type that gives cleaner type signatures. -}

module Data.Closed (
    -- * Closed
    C
    ) where

-- | The empty type, denoting a \'@C@\'losed end
data C = C -- Not exported, but I include it to avoid EmptyDataDecls
