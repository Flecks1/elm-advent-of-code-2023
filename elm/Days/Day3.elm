module Days.Day3 exposing (first, second)

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Puzzle
import Utils.Various exposing (iff)


first : Puzzle.Solution
first input =
    let
        { parts, symbols } =
            makeMap input

        anySymbolIsInRange x1 x2 y1 y2 =
            List.range x1 x2 |> List.any (\x -> List.range y1 y2 |> List.any (\y -> Dict.member ( x, y ) symbols))
    in
    parts
        |> List.filterMap (\{ number, x1, x2, y1, y2 } -> iff (anySymbolIsInRange x1 x2 y1 y2) (Just number) Nothing)
        |> List.sum
        |> String.fromInt
        |> Ok


second : Puzzle.Solution
second input =
    let
        { parts, symbols } =
            makeMap input

        potentialGears =
            symbols |> Dict.foldl (\coordinates symbol gears -> iff (symbol == '*') (Dict.insert coordinates [] gears) gears) Dict.empty

        getGearsInRange x1 x2 y1 y2 gears =
            let
                yRange =
                    List.range y1 y2
            in
            List.range x1 x2 |> List.concatMap (\x -> yRange |> List.filterMap (\y -> iff (Dict.member ( x, y ) gears) (Just ( x, y )) Nothing))

        addPartToGear partNumber gear gears =
            Dict.get gear gears
                |> Maybe.unwrap gears
                    (\adjacentParts ->
                        if List.length adjacentParts == 2 then
                            Dict.remove gear gears

                        else
                            Dict.insert gear (partNumber :: adjacentParts) gears
                    )

        getAllActualGears { number, x1, x2, y1, y2 } gears =
            getGearsInRange x1 x2 y1 y2 gears
                |> List.foldl (addPartToGear number) gears

        sumAllGearRatios _ adjacentParts sum =
            case adjacentParts of
                [ firstPart, secondPart ] ->
                    sum + firstPart * secondPart

                _ ->
                    sum
    in
    parts
        |> List.foldl getAllActualGears potentialGears
        |> Dict.foldl sumAllGearRatios 0
        |> String.fromInt
        |> Ok


type alias Map =
    { parts : List Part
    , symbols : Dict ( Int, Int ) Char
    }


type alias Part =
    { number : Int, x1 : Int, x2 : Int, y1 : Int, y2 : Int }


makeMap : String -> Map
makeMap =
    let
        step char ({ x, y, currentNumber, parts, symbols } as accumulator) =
            if Char.isDigit char then
                { accumulator
                    | currentNumber =
                        currentNumber
                            |> Maybe.map (\part -> { part | number = part.number ++ String.fromChar char })
                            |> Maybe.orElse (Just { number = String.fromChar char, x = x, y = y })
                    , x = x + 1
                }

            else
                { accumulator
                    | x = iff (char == '\n') 0 (x + 1)
                    , y = iff (char == '\n') (y + 1) y
                    , symbols =
                        if char /= '\n' && char /= '.' then
                            symbols |> Dict.insert ( x, y ) char

                        else
                            symbols
                    , currentNumber = Nothing
                    , parts =
                        currentNumber
                            |> Maybe.andThen
                                (\part ->
                                    String.toInt part.number
                                        |> Maybe.map
                                            (\intValue ->
                                                { number = intValue
                                                , x1 = part.x - 1
                                                , x2 = part.x + String.length part.number
                                                , y1 = part.y - 1
                                                , y2 = part.y + 1
                                                }
                                            )
                                )
                            |> Maybe.unwrap parts (\part -> part :: parts)
                }
    in
    String.foldl step { x = 0, y = 0, currentNumber = Nothing, parts = [], symbols = Dict.empty }
        >> (\x -> { parts = x.parts, symbols = x.symbols })
