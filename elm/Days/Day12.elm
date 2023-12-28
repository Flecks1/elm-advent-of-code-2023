module Days.Day12 exposing (first, second)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Puzzle


first : Puzzle.Solution
first input =
    let
        records =
            parseConditionRecords input
    in
    records |> List.map getAmountOfPossiblePermutations |> List.sum |> String.fromInt |> Ok


second : Puzzle.Solution
second input =
    let
        records =
            parseConditionRecords input
                |> List.map
                    (\{ springs, groupings } ->
                        { springs = List.concat [ springs, [ '?' ], springs, [ '?' ], springs, [ '?' ], springs, [ '?' ], springs ]
                        , groupings = List.concat [ groupings, groupings, groupings, groupings, groupings ]
                        }
                    )
    in
    records |> List.map getAmountOfPossiblePermutations |> List.sum |> String.fromInt |> Ok


getAmountOfPossiblePermutations : Record -> Int
getAmountOfPossiblePermutations { springs, groupings } =
    let
        ( springsArray, groupingsArray ) =
            ( Array.fromList springs, Array.fromList groupings )

        ( springsLength, groupingsLength ) =
            ( Array.length springsArray, Array.length groupingsArray )

        recursiveHelper index groupIndex groupSize =
            if index < springsLength then
                case Array.get index springsArray of
                    -- Operational spring
                    Just '.' ->
                        if groupSize == 0 then
                            -- If we hadn't started any damaged group yet, this is fine
                            recursiveHelper (index + 1) groupIndex groupSize

                        else if not (Array.get groupIndex groupingsArray == Just groupSize) then
                            -- The current damaged group is not fulfilled, stop here
                            0

                        else
                            -- The current damaged group is fulfilled, handle the next one
                            recursiveHelper (index + 1) (groupIndex + 1) 0

                    -- Damaged spring
                    Just '#' ->
                        if groupIndex >= groupingsLength || Array.get groupIndex groupingsArray == Just groupSize then
                            -- We weren't expecting an additional damaged spring
                            0

                        else
                            -- Increment the size of the current group
                            recursiveHelper (index + 1) groupIndex (groupSize + 1)

                    -- Unknown condition spring
                    _ ->
                        let
                            operationalAttempt =
                                if groupSize == 0 then
                                    -- If we haven't started a group yet, we can try treating this spring as operational
                                    recursiveHelper (index + 1) groupIndex groupSize

                                else if Array.get groupIndex groupingsArray == Just groupSize then
                                    -- If we already have the proper size for the current group we can instead treat it as operational so we can handle the next group
                                    recursiveHelper (index + 1) (groupIndex + 1) 0

                                else
                                    0

                            damagedAttempt =
                                -- If the current damaged group size isn't reached, we can try handling this spring as damaged
                                if groupIndex < groupingsLength && (Array.get groupIndex groupingsArray |> Maybe.unwrap False ((<) groupSize)) then
                                    recursiveHelper (index + 1) groupIndex (groupSize + 1)

                                else
                                    0
                        in
                        damagedAttempt + operationalAttempt

            else if groupIndex >= groupingsLength || (groupIndex == groupingsLength - 1 && Array.get groupIndex groupingsArray == Just groupSize) then
                -- Either we've already fullfilled the last group, or the last group ends at the end of the row
                1

            else
                0
    in
    recursiveHelper 0 0 0


type alias Condition =
    Char


type alias Position =
    Int


type alias Record =
    { springs : List Char
    , groupings : List Int
    }


parseConditionRecords : String -> List Record
parseConditionRecords =
    String.split "\n"
        >> List.filterMap
            (\line ->
                case String.split " " line of
                    [ springs, groupings ] ->
                        Just
                            { springs = springs |> String.toList
                            , groupings = groupings |> String.split "," |> List.filterMap String.toInt
                            }

                    _ ->
                        Nothing
            )
