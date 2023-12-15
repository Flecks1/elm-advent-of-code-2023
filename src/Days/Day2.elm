module Days.Day2 exposing (first, second)

import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle


first : Puzzle.Solution
first input =
    let
        respectsTheMaximumAmountsOfCube colors =
            colors.red <= 12 && colors.green <= 13 && colors.blue <= 14
    in
    parseGames input
        |> Result.map
            (List.filterMap
                (\game ->
                    if respectsTheMaximumAmountsOfCube (getMaxRevealedAmounts game) then
                        Just game.id

                    else
                        Nothing
                )
                >> List.sum
                >> String.fromInt
            )


second : Puzzle.Solution
second input =
    parseGames input
        |> Result.map
            (List.map (getMaxRevealedAmounts >> getPower)
                >> List.sum
                >> String.fromInt
            )


type alias Game =
    { id : Int, reveals : List (List ( Int, Color )) }


type Color
    = Red
    | Green
    | Blue


type alias Colors =
    { red : Int, green : Int, blue : Int }


getPower : Colors -> Int
getPower colors =
    colors.red * colors.green * colors.blue


getMaxRevealedAmounts : Game -> Colors
getMaxRevealedAmounts { reveals } =
    let
        revealedColorStep ( amount, color ) colors =
            case color of
                Red ->
                    if amount > colors.red then
                        { colors | red = amount }

                    else
                        colors

                Green ->
                    if amount > colors.green then
                        { colors | green = amount }

                    else
                        colors

                Blue ->
                    if amount > colors.blue then
                        { colors | blue = amount }

                    else
                        colors

        revealStep revealedColors colors =
            revealedColors |> List.foldl revealedColorStep colors
    in
    reveals |> List.foldl revealStep (Colors 0 0 0)


parseGames : String -> Result String (List Game)
parseGames input =
    let
        parseColor : Parser Color
        parseColor =
            Parser.oneOf
                [ Parser.token "red" |> Parser.map (\_ -> Red)
                , Parser.token "green" |> Parser.map (\_ -> Green)
                , Parser.token "blue" |> Parser.map (\_ -> Blue)
                ]

        parseRevealedColor : Parser ( Int, Color )
        parseRevealedColor =
            Parser.succeed Tuple.pair
                |. Parser.spaces
                |= Parser.int
                |. Parser.spaces
                |= parseColor

        reverseIf : Bool -> List a -> List a
        reverseIf pred list =
            if pred then
                List.reverse list

            else
                list

        addToLastReveal : ( Int, Color ) -> Bool -> List (List ( Int, Color )) -> List (List ( Int, Color ))
        addToLastReveal revealedColor isLastColorInReveal reveals =
            let
                newReveals =
                    case reveals of
                        reveal :: rest ->
                            reverseIf isLastColorInReveal (revealedColor :: reveal) :: rest

                        [] ->
                            [ [ revealedColor ] ]
            in
            if isLastColorInReveal then
                [] :: newReveals

            else
                newReveals

        cleanupReveals : List (List ( Int, Color )) -> List (List ( Int, Color ))
        cleanupReveals =
            List.uncons >> Maybe.map (Tuple.second >> List.reverse) >> Maybe.withDefault []

        parseReveals : Parser (List (List ( Int, Color )))
        parseReveals =
            Parser.loop []
                (\reveals ->
                    Parser.oneOf
                        [ Parser.succeed (\revealedColor -> Loop (addToLastReveal revealedColor False reveals))
                            |= parseRevealedColor
                            |. Parser.symbol ","
                            |> Parser.backtrackable
                        , Parser.succeed (\revealedColor -> Loop (addToLastReveal revealedColor True reveals))
                            |= parseRevealedColor
                            |. Parser.symbol ";"
                            |> Parser.backtrackable
                        , Parser.succeed (\revealedColor -> Done (addToLastReveal revealedColor True reveals |> cleanupReveals))
                            |= parseRevealedColor
                            |. Parser.symbol "\n"
                            |> Parser.backtrackable
                        ]
                )

        parseGame : Parser Game
        parseGame =
            Parser.succeed Game
                |. Parser.keyword "Game"
                |. Parser.spaces
                |= Parser.int
                |. Parser.symbol ":"
                |= parseReveals
    in
    input
        |> Parser.run
            (Parser.loop []
                (\games ->
                    Parser.oneOf
                        [ Parser.succeed (\game -> Loop (game :: games))
                            |= parseGame
                        , Parser.end |> Parser.map (\_ -> Done games)
                        ]
                )
            )
        |> Result.mapError (Debug.log "deadEnds" >> Parser.deadEndsToString)
