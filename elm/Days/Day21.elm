module Days.Day21 exposing (first, second)

import List.Extra as List
import Puzzle
import Set exposing (Set)


first : Puzzle.Solution
first input =
    let
        { rocks, start, xBoundary, yBoundary } =
            makePuzzleModel input

        isWithinBoundaries ( x, y ) =
            x >= 0 && x <= xBoundary && y >= 0 && y <= yBoundary

        addAdjacentPlots ( x, y ) reachedPlots =
            [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                |> List.filter (\position -> isWithinBoundaries position && not (Set.member position rocks))
                |> List.foldl Set.insert reachedPlots

        loop remainingSteps reachedPlots =
            if remainingSteps > 0 then
                let
                    _ =
                        Debug.log ""
                            { a_steps = 64 - remainingSteps
                            , b_reachedPlots = Set.size reachedPlots |> String.fromInt |> Ok
                            }

                    newReachedPlots =
                        reachedPlots |> Set.foldl addAdjacentPlots Set.empty
                in
                loop (remainingSteps - 1) newReachedPlots

            else
                Set.size reachedPlots |> String.fromInt |> Ok
    in
    loop 64 (Set.singleton start)


second : Puzzle.Solution
second input =
    let
        { rocks, start, xBoundary, yBoundary } =
            makePuzzleModel input

        addAdjacentPlots ( x, y ) reachedPlots =
            [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                |> List.filter
                    (Tuple.mapBoth (modBy xBoundary) (modBy yBoundary)
                        >> (\position -> not (Set.member position rocks))
                    )
                |> List.foldl Set.insert reachedPlots

        loop remainingSteps reachedPlots =
            if remainingSteps > 0 then
                let
                    newReachedPlots =
                        reachedPlots |> Set.foldl addAdjacentPlots Set.empty
                in
                loop (remainingSteps - 1) newReachedPlots

            else
                Set.size reachedPlots |> String.fromInt |> Ok
    in
    loop 64 (Set.singleton start)


type alias Point =
    ( Int, Int )


type alias PuzzleModel =
    { rocks : Set Point
    , start : Point
    , xBoundary : Int
    , yBoundary : Int
    }


makePuzzleModel : String -> PuzzleModel
makePuzzleModel =
    let
        handleSymbol x y symbol ({ xBoundary, yBoundary, rocks } as model) =
            let
                newModel =
                    { model | xBoundary = max x xBoundary, yBoundary = max y yBoundary }
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
    String.split "\n" >> List.indexedFoldl handleRow { rocks = Set.empty, start = ( 0, 0 ), xBoundary = 0, yBoundary = 0 }
