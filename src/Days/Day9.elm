module Days.Day9 exposing (first, second)

import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Set
import Utils.Parser as Parser


first : Puzzle.Solution
first =
    parseSequences >> Result.map (List.map getNextNumberInSequence >> List.sum >> String.fromInt)


second : Puzzle.Solution
second =
    parseSequences >> Result.map (List.map (List.reverse >> getNextNumberInSequence) >> List.sum >> String.fromInt)


getNextNumberInSequence : List Int -> Int
getNextNumberInSequence sequence =
    let
        -- Note: We parse the sequences and store them in reverse order to facilitate the summing of the last numbers in the decomposed sequences down the line
        getAllDifferences x ( lastNumber, differences ) =
            ( Just x, lastNumber |> Maybe.unwrap differences (\y -> (x - y) :: differences) )

        getDifferencesInSequence =
            List.foldr getAllDifferences ( Nothing, [] ) >> Tuple.second

        recursiveHelper continuation sequence_ =
            let
                differences =
                    getDifferencesInSequence sequence_
            in
            case ( Set.fromList differences |> Set.toList, differences ) of
                ( [ x ], _ ) ->
                    x |> continuation

                ( _, x :: _ ) ->
                    differences |> recursiveHelper (\y -> x + y |> continuation)

                _ ->
                    0
    in
    case sequence of
        x :: _ ->
            sequence |> recursiveHelper (\y -> x + y)

        _ ->
            0



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
                , Parser.succeed (\number -> number :: numbers |> Done)
                    |= potentiallyNegativeIntParser
                    |. Parser.symbol "\n"
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
