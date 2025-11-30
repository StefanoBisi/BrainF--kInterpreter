module BFUtilities (
    putInList,
    rollUp,
    rollDown
) where

putInList :: [a] -> Int -> a -> [a]
putInList list position value =
    take position list ++ [value] ++ drop (succ position) list

-- Increment a value, or set it to its type's minimum if the maximum was reached 
rollUp :: (Bounded a, Enum a, Eq a) => a -> a
rollUp value =
    if value == maxBound
        then minBound
        else succ value

-- Decrement a value, or set it to its type's maximum if the minimum was reached
rollDown :: (Bounded a, Enum a, Eq a) => a -> a
rollDown value =
    if value == minBound
        then maxBound
        else pred value