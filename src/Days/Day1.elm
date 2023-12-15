module Days.Day1 exposing (first, second)

import Parser
import Parser.Advanced
import Puzzle
import Regex exposing (Regex)


first : Puzzle.Solution
first input =
    Regex.split whitespaceRegex input
        |> List.map getNumberForLine_first
        |> List.sum
        |> String.fromInt
        |> Ok


second : Puzzle.Solution
second input =
    Regex.split whitespaceRegex input
        |> List.map getNumberForLine_second
        |> List.sum
        |> String.fromInt
        |> Ok


whitespaceRegex : Regex
whitespaceRegex =
    Regex.fromString "\\s+" |> Maybe.withDefault Regex.never


getNumberForLine_first : String -> Int
getNumberForLine_first line =
    let
        onlyDigits =
            line |> String.filter Char.isDigit

        firstDigit =
            onlyDigits |> String.left 1

        secondDigit =
            onlyDigits |> String.right 1
    in
    firstDigit ++ secondDigit |> String.toInt |> Maybe.withDefault 0


parseSpelledOutDigit : String -> Maybe ( Int, Int )
parseSpelledOutDigit string =
    let
        parser =
            [ ( "one", 2, 1 ), ( "two", 2, 2 ), ( "three", 4, 3 ), ( "four", 4, 4 ), ( "five", 3, 5 ), ( "six", 3, 6 ), ( "seven", 4, 7 ), ( "eight", 4, 8 ), ( "nine", 3, 9 ) ]
                |> List.map (\( spelledOut, howManyCharactersToSkip, number ) -> Parser.token spelledOut |> Parser.map (\_ -> ( howManyCharactersToSkip, number )) |> Parser.backtrackable)
                |> Parser.oneOf
    in
    Parser.run parser string |> Result.toMaybe


getNumberForLine_second : String -> Int
getNumberForLine_second line =
    let
        getAllNumbers remainingCharacters foundNumbers =
            case String.uncons remainingCharacters of
                Just ( firstChar, rest ) ->
                    if Char.isDigit firstChar then
                        getAllNumbers rest (foundNumbers ++ String.fromChar firstChar)

                    else
                        case parseSpelledOutDigit remainingCharacters of
                            Just ( howManyCharactersToSkip, number ) ->
                                getAllNumbers (String.dropLeft howManyCharactersToSkip remainingCharacters) (foundNumbers ++ String.fromInt number)

                            Nothing ->
                                getAllNumbers rest foundNumbers

                Nothing ->
                    foundNumbers

        allNumbers =
            getAllNumbers line ""

        firstDigit =
            allNumbers |> String.left 1

        secondDigit =
            allNumbers |> String.right 1
    in
    firstDigit ++ secondDigit |> String.toInt |> Maybe.withDefault 0
