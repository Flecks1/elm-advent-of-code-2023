module Days.Day11 exposing (first, second)

import List.Extra as List
import Puzzle
import Set


first : Puzzle.Solution
first =
    puzzleHelper 1


second : Puzzle.Solution
second =
    puzzleHelper 999999


puzzleHelper : Int -> Puzzle.Solution
puzzleHelper expansionRate input =
    let
        ( galaxies, maximumX, maximumY ) =
            makeMap input

        allValuesUpTo n =
            List.range 0 n |> Set.fromList

        ( columnsWithoutGalaxies, rowsWithoutGalaxies ) =
            List.unzip galaxies
                |> Tuple.mapBoth
                    (Set.fromList >> Set.diff (allValuesUpTo maximumX) >> Set.toList)
                    (Set.fromList >> Set.diff (allValuesUpTo maximumY) >> Set.toList)

        expandSpace ( x, y ) =
            ( x + (columnsWithoutGalaxies |> List.filter (\x1 -> x1 < x) |> List.length |> (*) expansionRate)
            , y + (rowsWithoutGalaxies |> List.filter (\y1 -> y1 < y) |> List.length |> (*) expansionRate)
            )

        getDistance ( ( x1, y1 ), ( x2, y2 ) ) =
            abs (x2 - x1) + abs (y2 - y1)
    in
    galaxies
        |> List.map expandSpace
        |> List.uniquePairs
        |> List.map getDistance
        |> List.sum
        |> String.fromInt
        |> Ok


type alias Coordinates =
    ( Int, Int )



-- Parsing


makeMap : String -> ( List Coordinates, Int, Int )
makeMap =
    let
        handleSymbol x y symbol ( galaxies, maximumX, maximumY ) =
            ( if symbol == '#' then
                ( x, y ) :: galaxies

              else
                galaxies
            , max maximumX x
            , max maximumY y
            )

        handleRow y row state =
            String.toList row |> List.indexedFoldl (\x symbol -> handleSymbol x y symbol) state
    in
    String.split "\n" >> List.indexedFoldl handleRow ( [], 0, 0 )
