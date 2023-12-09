module Days.DayOne exposing (first)

import Puzzle
import Regex


first : Puzzle.Solution
first input =
    let
        whitespaces =
            Regex.fromString "\\s+" |> Maybe.withDefault Regex.never
    in
    input
        |> Regex.split whitespaces
        |> List.map getNumberForLine
        |> List.sum
        |> String.fromInt
        |> Ok


getNumberForLine : String -> Int
getNumberForLine line =
    let
        onlyDigits =
            line |> String.filter Char.isDigit

        firstDigit =
            onlyDigits |> String.left 1

        secondDigit =
            onlyDigits |> String.right 1
    in
    firstDigit ++ secondDigit |> String.toInt |> Maybe.withDefault 0
