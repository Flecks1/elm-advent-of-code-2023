module Utils.NumberSequenceSolver exposing (getDecomposedSequence)

import List.Extra as List
import Maybe.Extra as Maybe
import Utils.Various exposing (iff, isZero)


{-| For a sequence of numbers, decomposes the sequence in a list of numbers that allows to guess the next numbers in the sequence
For example when receiving the following list

    [ 1, 3, 6, 10, 15 ]

We decompose the sequence into the following sub-sequences from recursively calculating the differences between each numbers in the sequence

    [ 1, 3, 6, 10, 15 ]
    [ 2, 3, 4, 5 ]
    [ 1, 1, 1 ]
    [ 0, 0 ]

For this example the function would return `Just [ 1, 5, 15 ]`
If we can't reach a sub-sequences only composed of zeroes, we return `Nothing`

This list can be summed to get the next value in the list, of folded over any number of time to get every next values.

-}
getDecomposedSequence : List number -> Maybe (List number)
getDecomposedSequence sequence =
    let
        getAllDifferences x ( lastNumber, calculatedDifferences ) =
            ( Just x, lastNumber |> Maybe.unwrap calculatedDifferences (\y -> (x - y) :: calculatedDifferences) )

        extractValueIfAllNumbersAreTheSame =
            Nothing |> List.stoppableFoldl (\n -> Maybe.unwrap (List.Continue (Just n)) (\i -> iff (i == n) (List.Continue (Just n)) (List.Stop Nothing)))

        recursiveHelper cont reversedSequence =
            let
                differences =
                    reversedSequence |> List.foldr getAllDifferences ( Nothing, [] ) |> Tuple.second
            in
            case ( extractValueIfAllNumbersAreTheSame differences |> Maybe.unwrap False isZero, differences ) of
                -- All differences are 0, return an empty list (since we don't really care about the 0 value in the decomposed sequence)
                ( True, _ ) ->
                    [] |> cont

                ( _, x :: _ ) ->
                    differences |> recursiveHelper (\decomposedSequence -> x :: decomposedSequence |> cont)

                _ ->
                    Nothing
    in
    -- Note: We reverse the sequence to facilitate the accessing of the last numbers in the decomposed sequences down the line, then reverse it back before returning the final result
    case List.reverse sequence of
        (x :: _) as reversedSequence ->
            recursiveHelper (\decomposedSequence -> List.reverse (x :: decomposedSequence) |> Just) reversedSequence

        _ ->
            Nothing
