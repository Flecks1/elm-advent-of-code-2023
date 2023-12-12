module Days.DayFour exposing (first, second)

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Set exposing (Set)


first : Puzzle.Solution
first input =
    parseInput input |> Result.map (List.map calculateCardPoints >> List.sum >> String.fromInt)


second : Puzzle.Solution
second input =
    let
        getInitialCardAmounts =
            List.foldl (\{ number } -> Dict.insert number 1) Dict.empty

        countCards card cardAmounts =
            let
                amountOfWinningNumbers =
                    getAmountOfWinningNumbers card

                incrementNextCards nextCardNumber =
                    Dict.update nextCardNumber (Maybe.map2 (+) (Dict.get card.number cardAmounts))
            in
            if amountOfWinningNumbers > 0 then
                List.range (card.number + 1) (card.number + amountOfWinningNumbers)
                    |> List.foldl incrementNextCards cardAmounts

            else
                cardAmounts
    in
    parseInput input |> Result.map (\cards -> cards |> List.foldl countCards (getInitialCardAmounts cards) |> Dict.values |> List.sum |> String.fromInt)


type alias Card =
    { number : Int, winningNumbers : Set Int, numbers : Set Int }


getAmountOfWinningNumbers : Card -> Int
getAmountOfWinningNumbers { winningNumbers, numbers } =
    Set.size (Set.intersect winningNumbers numbers)


calculateCardPoints : Card -> Int
calculateCardPoints card =
    let
        amountOfWinningNumbers =
            getAmountOfWinningNumbers card
    in
    if amountOfWinningNumbers > 0 then
        2 ^ (amountOfWinningNumbers - 1)

    else
        0


cardParser : Parser Card
cardParser =
    let
        winningNumbersParser =
            Parser.loop Set.empty
                (\winningNumbers ->
                    Parser.oneOf
                        [ Parser.succeed (\number -> Loop (Set.insert number winningNumbers))
                            |. Parser.spaces
                            |= Parser.int
                            |> Parser.backtrackable
                        , Parser.succeed (Done winningNumbers)
                            |. Parser.spaces
                            |. Parser.symbol "|"
                            |> Parser.backtrackable
                        ]
                )

        numbersParser =
            Parser.loop Set.empty
                (\numbers ->
                    Parser.oneOf
                        [ Parser.succeed (\number -> Loop (Set.insert number numbers))
                            |. Parser.spaces
                            |= Parser.int
                            |> Parser.backtrackable
                        , Parser.succeed (Done numbers)
                            |. Parser.symbol "\n"
                            |> Parser.backtrackable
                        ]
                )
    in
    Parser.succeed Card
        |. Parser.keyword "Card"
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol ":"
        |= winningNumbersParser
        |= numbersParser


parseInput : String -> Result String (List Card)
parseInput =
    Parser.run
        (Parser.loop []
            (\cards ->
                Parser.oneOf
                    [ Parser.succeed (\card -> Loop (card :: cards))
                        |= cardParser
                    , Parser.end |> Parser.map (\_ -> Done (List.reverse cards))
                    ]
            )
        )
        >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
