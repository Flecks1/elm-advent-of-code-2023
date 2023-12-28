module Days.Day15 exposing (first, second)

import Dict exposing (Dict)
import Html exposing (label)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Utils.Parser as Parser


first : Puzzle.Solution
first =
    let
        stepParser =
            Parser.string ((/=) ',') |> Parser.map hashValue

        parser =
            Parser.succeed (+)
                |= stepParser
                |= Parser.loop 0
                    (\value ->
                        Parser.oneOf
                            [ Parser.succeed (\stepValue -> stepValue + value |> Loop)
                                |. Parser.symbol ","
                                |= stepParser
                                |> Parser.backtrackable
                            , Parser.end |> Parser.map (\_ -> Done value)
                            ]
                    )
    in
    Parser.run parser
        >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
        >> Result.map String.fromInt


second : Puzzle.Solution
second =
    Parser.run boxesSetupParser
        >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
        >> Result.map (calculateFocusingPower >> String.fromInt)


boxesSetupParser : Parser (Dict BoxNumber (List Lens))
boxesSetupParser =
    let
        operationParser =
            Parser.oneOf
                [ Parser.symbol "-" |> Parser.map (\_ -> Remove) |> Parser.backtrackable
                , Parser.succeed Add
                    |. Parser.symbol "="
                    |= Parser.int
                ]

        labelParser =
            Parser.string (\char -> char /= '-' && char /= '=')

        stepParser =
            Parser.succeed Tuple.pair |= labelParser |= operationParser
    in
    stepParser
        |> Parser.map (\step -> Dict.empty |> doStep step)
        |> Parser.andThen
            (\boxesAfterFirstStep ->
                Parser.loop boxesAfterFirstStep
                    (\boxesSetup ->
                        Parser.oneOf
                            [ Parser.succeed (\step -> boxesSetup |> doStep step |> Loop)
                                |. Parser.symbol ","
                                |= stepParser
                                |> Parser.backtrackable
                            , Parser.end |> Parser.map (\_ -> Done boxesSetup)
                            ]
                    )
            )


calculateFocusingPower : Dict BoxNumber (List Lens) -> Int
calculateFocusingPower =
    let
        incrementFocusingPower boxNumber lenses currentPower =
            lenses |> List.indexedFoldl (\index ( _, focalLength ) acc -> (boxNumber + 1) * (index + 1) * focalLength + acc) currentPower
    in
    Dict.foldl incrementFocusingPower 0


hashValue : String -> Int
hashValue =
    String.foldl (\char n -> (n + Char.toCode char) * 17 |> modBy 256) 0


doStep : ProcessStep -> Dict BoxNumber (List Lens) -> Dict BoxNumber (List Lens)
doStep ( lensLabel, operation ) boxesSetup =
    let
        boxNumber =
            hashValue lensLabel

        operationFunction =
            case operation of
                Remove ->
                    List.filter (Tuple.first >> (/=) lensLabel)

                Add focalLength ->
                    \lenses ->
                        if List.any (Tuple.first >> (==) lensLabel) lenses then
                            lenses |> List.updateIf (Tuple.first >> (==) lensLabel) (Tuple.mapSecond (\_ -> focalLength))

                        else
                            lenses ++ [ ( lensLabel, focalLength ) ]
    in
    boxesSetup |> Dict.update boxNumber (Maybe.withDefault [] >> operationFunction >> Just)


type alias ProcessStep =
    ( Label, Operation )


type Operation
    = Remove
    | Add FocalLength


type alias Lens =
    ( Label, FocalLength )


type alias BoxNumber =
    Int


type alias Label =
    String


type alias FocalLength =
    Int
