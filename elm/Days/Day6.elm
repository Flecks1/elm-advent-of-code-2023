module Days.Day6 exposing (first, second)

import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle


first : Puzzle.Solution
first input =
    parseRaces input |> Result.map (List.foldl (calculateAmountOfWaysToWin >> (*)) 1 >> String.fromInt)


second : Puzzle.Solution
second input =
    parseRace input |> Result.map (calculateAmountOfWaysToWin >> String.fromInt)


calculateAmountOfWaysToWin : Race -> Int
calculateAmountOfWaysToWin ( time, distance ) =
    let
        {-
           Example:
           30 milliseconds, 200 millimeters

                      -b ± √(b² - 4ac)
           ---->      ________________
                             2a


           ---->      x(30 - x) > 200

           ---->    x² - 30x + 200 = 0

                      30 ± √(900 - 800)
           ---->      ________________
                             2

               x = 10 and x = 20 makes y = 200

                   11 and 19 makes y > 200
        -}
        answer_square_root_b2_minus_4ac =
            (time ^ 2 - (4 * distance)) |> toFloat |> sqrt

        adjustBoundary roundUpOrDownIfDecimal amountToIncrementIfInteger n =
            if toFloat (round n) == n then
                amountToIncrementIfInteger + n |> round

            else
                roundUpOrDownIfDecimal n

        lowerBoundary =
            ((toFloat time - answer_square_root_b2_minus_4ac) / 2)
                |> adjustBoundary ceiling 1.0

        upperBoundary =
            ((toFloat time + answer_square_root_b2_minus_4ac) / 2)
                |> adjustBoundary floor -1.0
    in
    upperBoundary - lowerBoundary + 1


type alias Race =
    ( Time, Distance )


type alias Time =
    Int


type alias Distance =
    Int


intListParser : Parser (List Int)
intListParser =
    Parser.loop []
        (\ints ->
            Parser.oneOf
                [ Parser.succeed (\int -> int :: ints |> Loop)
                    |. Parser.spaces
                    |= Parser.int
                    |> Parser.backtrackable
                , Parser.oneOf [ Parser.symbol "\n", Parser.end ]
                    |> Parser.map (\_ -> ints |> List.reverse |> Done)
                    |> Parser.backtrackable
                ]
        )


parseRaces : String -> Result String (List Race)
parseRaces =
    let
        parser =
            Parser.succeed List.zip
                |. Parser.token "Time:"
                |= intListParser
                |. Parser.token "Distance:"
                |= intListParser
    in
    Parser.run parser >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))


badlyWrittenIntParser : Parser Int
badlyWrittenIntParser =
    Parser.loop ""
        (\string ->
            Parser.oneOf
                [ Parser.succeed (\int -> string ++ String.fromInt int |> Loop)
                    |. Parser.spaces
                    |= Parser.int
                    |> Parser.backtrackable
                , Parser.oneOf [ Parser.symbol "\n", Parser.end ]
                    |> Parser.map (\_ -> string |> String.toInt |> Maybe.withDefault 1 |> Done)
                    |> Parser.backtrackable
                ]
        )


parseRace : String -> Result String Race
parseRace =
    let
        parser =
            Parser.succeed Tuple.pair
                |. Parser.token "Time:"
                |= badlyWrittenIntParser
                |. Parser.token "Distance:"
                |= badlyWrittenIntParser
    in
    Parser.run parser >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
