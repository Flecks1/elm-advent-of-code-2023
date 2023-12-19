module Days.Day8 exposing (first, second)

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step(..))
import Puzzle
import Result.Extra as Result
import String exposing (right)
import Utils.Parser as Parser


first : Puzzle.Solution
first =
    let
        start =
            "AAA"

        end =
            "ZZZ"

        stopIfWeReachedTheEnd state =
            if state.node == end then
                List.Stop { state | done = True }

            else
                List.Continue state

        step tree move ({ node, steps } as state) =
            { state | node = node |> navigateTree tree move, steps = steps + 1 }
                |> stopIfWeReachedTheEnd

        loop state ({ moves, tree } as x) =
            let
                newState =
                    moves |> List.stoppableFoldl (step tree) state
            in
            if newState.done then
                newState.steps |> String.fromInt

            else
                loop newState x
    in
    parsePuzzleInput
        >> Result.map (loop { steps = 0, node = start, done = False })


second : Puzzle.Solution
second =
    let
        getAllStartNodes =
            Dict.foldr
                (\node _ paths ->
                    if node |> String.endsWith "A" then
                        { node = node, cycle = Nothing } :: paths

                    else
                        paths
                )
                []

        stopIfWeFoundAllCycles state =
            let
                checkIfCycleWasFound ({ node, cycle } as pathState) =
                    if String.endsWith "Z" node then
                        { pathState | cycle = cycle |> Maybe.orElse (Just state.steps) }

                    else
                        pathState

                newState =
                    { state | paths = state.paths |> List.map checkIfCycleWasFound }
            in
            if newState.paths |> List.all (.cycle >> Maybe.isJust) then
                List.Stop newState

            else
                List.Continue newState

        step tree move ({ steps, paths } as state) =
            { state
                | paths = paths |> List.map (\pathState -> { pathState | node = pathState.node |> navigateTree tree move })
                , steps = steps + 1
            }
                |> stopIfWeFoundAllCycles

        -- After observing the cycles when you run the path for each start node, all of them seem to follow a cycle that is a multiple of the amount of steps in the path
        -- So we can divide all the cycles length by the amount of steps, multiply them together, than multiply the result by the amount of steps again to get the amount of steps at which they all coincide
        loop ({ moves, tree } as x) state =
            let
                newState =
                    moves |> List.stoppableFoldl (step tree) state
            in
            case newState.paths |> Maybe.traverse .cycle of
                Just cycles ->
                    let
                        movesAmount =
                            List.length moves
                    in
                    cycles |> List.map (\n -> n // movesAmount) |> List.product |> (*) movesAmount |> String.fromInt

                Nothing ->
                    loop x newState
    in
    parsePuzzleInput
        >> Result.map (\puzzleInput -> loop puzzleInput { steps = 0, paths = getAllStartNodes puzzleInput.tree, done = False })


navigateTree : Tree -> Move -> Node -> Node
navigateTree tree move node =
    case ( move, Dict.get node tree ) of
        ( 'L', Just ( left, _ ) ) ->
            left

        ( 'R', Just ( _, right ) ) ->
            right

        _ ->
            -- This should never happen
            Debug.log "Something went terribly wrong and you stayed on" node


type alias PuzzleInput =
    { moves : List Move
    , tree : Tree
    }


type alias Node =
    String


type alias Move =
    Char


type alias Branch =
    ( Node, Node )


type alias Tree =
    Dict Node Branch


treeParser : Parser Tree
treeParser =
    let
        nodeParser =
            Parser.string Char.isAlphaNum
    in
    Parser.loop Dict.empty
        (\tree ->
            Parser.oneOf
                [ Parser.succeed (\node left right -> tree |> Dict.insert node ( left, right ) |> Loop)
                    |. Parser.spaces
                    |= nodeParser
                    |. Parser.spaces
                    |. Parser.symbol "="
                    |. Parser.spaces
                    |. Parser.symbol "("
                    |= nodeParser
                    |. Parser.symbol ","
                    |. Parser.spaces
                    |= nodeParser
                    |. Parser.symbol ")"
                , Parser.end |> Parser.map (\_ -> Done tree)
                ]
        )


parsePuzzleInput : String -> Result String PuzzleInput
parsePuzzleInput =
    let
        parser =
            Parser.succeed PuzzleInput
                |= (Parser.string (\char -> char == 'L' || char == 'R') |> Parser.map String.toList)
                |. Parser.spaces
                |= treeParser
    in
    Parser.run parser >> Result.mapError (Debug.log "deadEnds" >> (\_ -> "Parser error, look in the console"))
