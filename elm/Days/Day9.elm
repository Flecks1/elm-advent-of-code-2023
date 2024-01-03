module Days.Day9 exposing (first, second)

import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Utils.NumberSequenceSolver
import Utils.Parser as Parser


first : Puzzle.Solution
first =
    parseSequences >> Result.map (List.map calculateNextNumberHelper >> List.sum >> String.fromInt)


second : Puzzle.Solution
second =
    parseSequences >> Result.map (List.map (List.reverse >> calculateNextNumberHelper) >> List.sum >> String.fromInt)


calculateNextNumberHelper : List Int -> Int
calculateNextNumberHelper =
    Utils.NumberSequenceSolver.getDecomposedSequence >> Maybe.unwrap 0 List.sum



-- Parsing


potentiallyNegativeIntParser : Parser Int
potentiallyNegativeIntParser =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
            |> Parser.backtrackable
        , Parser.int |> Parser.backtrackable
        ]


sequenceParser : Parser (List Int)
sequenceParser =
    Parser.loop []
        (\numbers ->
            Parser.oneOf
                [ Parser.succeed (\number -> number :: numbers |> Loop)
                    |= potentiallyNegativeIntParser
                    |. Parser.symbol " "
                    |> Parser.backtrackable
                , Parser.succeed (\number -> number :: numbers |> List.reverse |> Done)
                    |= potentiallyNegativeIntParser
                    |. Parser.oneOf [ Parser.symbol "\n", Parser.end ]
                    |> Parser.backtrackable
                ]
        )


parseSequences : String -> Result String (List (List Int))
parseSequences =
    let
        parser =
            Parser.loop []
                (\sequences ->
                    Parser.oneOf
                        [ sequenceParser
                            |> Parser.map (\sequence -> sequence :: sequences |> Loop)
                            |> Parser.backtrackable
                        , Parser.end
                            |> Parser.map (\_ -> sequences |> List.reverse |> Done)
                            |> Parser.backtrackable
                        ]
                )
    in
    Parser.run parser >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
