module Utilities (
    putInList,
    rollUp,
    rollDown
) where

putInList :: [a] -> Int -> a -> [a]
putInList list position value =
    take position list ++ [value] ++ drop (succ position) list

rollUp :: (Bounded a, Enum a, Eq a) => a -> a
rollUp value =
    if value == maxBound
        then minBound
        else succ value

rollDown :: (Bounded a, Enum a, Eq a) => a -> a
rollDown value =
    if value == minBound
        then maxBound
        else pred value