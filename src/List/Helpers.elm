module List.Helpers exposing (find)

{-| Convenience functions for working with List


# Basics


# List transformations


# Folds


# Building lists


# Sublists


# Predicates


# Searching

@docs find


# Zipping


# Lift functions onto multiple lists of arguments


# Split to groups of given size

-}

import List exposing (..)


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ]
    --> Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest
