module Days.Day21 exposing (first, second)

import List.Extra as List
import Maybe.Extra as Maybe
import Puzzle
import Set exposing (Set)
import Utils.NumberSequenceSolver as NumberSequenceSolver exposing (getDecomposedSequence)


first : Puzzle.Solution
first input =
    helper 64 input


second : Puzzle.Solution
second input =
    helper 26501365 input


helper : Int -> Puzzle.Solution
helper maxSteps input =
    let
        { rocks, start, mapSize } =
            makePuzzleModel input

        halfOfMapSize =
            mapSize // 2

        remapPosition =
            -- Remap positions that are out of bounds into their position in the original map to allow going infinitely in all directions
            Tuple.mapBoth (modBy mapSize) (modBy mapSize)

        addAdjacentPlots ( x, y ) reachedPlots =
            [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                |> List.filter (remapPosition >> (\position -> not (Set.member position rocks)))
                |> List.foldl Set.insert reachedPlots

        findNextNumber i decomposedSequence =
            if i == 0 then
                List.last decomposedSequence |> Maybe.unwrap "0" String.fromInt |> Ok

            else
                findNextNumber (i - 1) (List.scanl1 (+) decomposedSequence)

        loop steps reachedPlots sequence =
            if steps <= maxSteps then
                let
                    newReachedPlots =
                        reachedPlots |> Set.foldl addAdjacentPlots Set.empty
                in
                -- 1) The start point is carefully placed in the center of the map for every input, and the shortest path to
                -- the side of the map is carefully made clear of any rocks so the border of the map will always be reached
                -- in a constant amount of steps.
                -- 2) The amount of steps we are trying to solve can be defined as x * mapSize + halfOfMapSize
                -- This allows us to calculate a very big amount of steps by simply building a sequence of amount of plots reached with whenever
                -- the amount of steps we walk get us a across a distance equal to map size. Whenever this sequence becomes solvable, we can
                -- simply get the decomposed sequence using the same algorithm from Day 9, then simply iterate x amount of times on the
                -- decomposed list without much complexity to get the expected amount of plots reached.
                if steps == halfOfMapSize || modBy mapSize (steps - halfOfMapSize) == 0 then
                    let
                        newSequence =
                            sequence ++ [ Set.size newReachedPlots ]

                        _ =
                            Debug.log ""
                                { a_steps = steps
                                , b_sequence = newSequence
                                }
                    in
                    case NumberSequenceSolver.getDecomposedSequence newSequence of
                        Just decomposedSequence ->
                            findNextNumber ((maxSteps - steps) // mapSize) decomposedSequence

                        Nothing ->
                            loop (steps + 1) newReachedPlots newSequence

                else
                    loop (steps + 1) newReachedPlots sequence

            else
                Set.size reachedPlots |> String.fromInt |> Ok
    in
    loop 1 (Set.singleton start) []


type alias Point =
    ( Int, Int )


type alias PuzzleModel =
    { rocks : Set Point
    , start : Point
    , mapSize : Int
    }


makePuzzleModel : String -> PuzzleModel
makePuzzleModel =
    let
        handleSymbol x y symbol ({ mapSize, rocks } as model) =
            let
                newModel =
                    { model | mapSize = max (x + 1) mapSize }
            in
            case symbol of
                '#' ->
                    { newModel | rocks = rocks |> Set.insert ( x, y ) }

                'S' ->
                    { newModel | start = ( x, y ) }

                _ ->
                    newModel

        handleRow y row model =
            String.toList row |> List.indexedFoldl (\x symbol -> handleSymbol x y symbol) model
    in
    String.split "\n" >> List.indexedFoldl handleRow { rocks = Set.empty, start = ( 0, 0 ), mapSize = 0 }
