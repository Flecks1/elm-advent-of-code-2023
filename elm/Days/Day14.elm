module Days.Day14 exposing (first, second)

import Dict exposing (Dict)
import Hash
import List.Extra as List
import Maybe.Extra as Maybe
import Puzzle


first : Puzzle.Solution
first =
    makeMap >> tiltNorth >> calculateLoad >> String.fromInt >> Ok


second : Puzzle.Solution
second =
    let
        amountOfCyclesExtrapolate =
            1000000000

        cycle =
            tiltNorth >> tiltWest >> tiltSouth >> tiltEast

        calculateHash =
            draw >> Hash.fromString

        maybeFindLoop i hashToLookFor remainingHashes =
            case remainingHashes of
                ( hash, _ ) :: rest ->
                    if hash == hashToLookFor then
                        Just i

                    else
                        maybeFindLoop (i - 1) hashToLookFor rest

                [] ->
                    Nothing

        loop i calculatedValues map =
            -- build up a reversed list of all calculated hashes and loads for each amount of cycles, starting from 0.
            -- after each cycle, check if the hashed map corresponds to any hash we've already seen.
            -- if we find a match, we've found a cycle, and then we can extrapolate at what index in that loop
            -- we will be at the amount of cycles we are looking for, we can then look up the load we calculated for
            -- that particular amount of loads and we've found the answer
            let
                newCalculatedHash =
                    calculateHash map
            in
            case calculatedValues |> maybeFindLoop (i - 1) newCalculatedHash of
                Just loopStartIndex ->
                    let
                        loopLength =
                            i - loopStartIndex

                        extrapolatedIndexInLoop =
                            (amountOfCyclesExtrapolate - loopStartIndex)
                                |> modBy loopLength

                        invertedIndex =
                            loopLength - extrapolatedIndexInLoop - 1
                    in
                    List.getAt invertedIndex calculatedValues
                        |> Maybe.unwrap 0 Tuple.second
                        |> String.fromInt
                        |> Ok

                Nothing ->
                    loop (i + 1) (( newCalculatedHash, calculateLoad map ) :: calculatedValues) (cycle map)

        -- Get a debug list of the hashes and loads for the first 200 cycles (the cycle seems to begin in that sample usually)
        -- loop i calculatedLoads map =
        --     if i < 200 then
        --         loop (i + 1) (( i, Hash.fromString (draw map) ) :: calculatedLoads) (cycle map)
        --     else
        --         calculatedLoads |> List.reverse |> Debug.toString |> Ok
    in
    makeMap >> loop 0 []


type alias FixedPositions =
    Dict Int Int


type alias DictFolderFunction key value acc =
    key -> value -> acc -> acc


readFixedY : Int -> Int -> Position -> FixedPositions -> Position
readFixedY boundary increment ( x, _ ) fixedPositions =
    ( x, fixedPositions |> Dict.get x |> Maybe.unwrap boundary ((+) increment) )


insertFixedY : Position -> FixedPositions -> FixedPositions
insertFixedY ( x, y ) fixedPositions =
    fixedPositions |> Dict.insert x y


readFixedX : Int -> Int -> Position -> FixedPositions -> Position
readFixedX boundary increment ( _, y ) fixedPositions =
    ( fixedPositions |> Dict.get y |> Maybe.unwrap boundary ((+) increment), y )


insertFixedX : Position -> FixedPositions -> FixedPositions
insertFixedX ( x, y ) fixedPositions =
    fixedPositions |> Dict.insert y x


tilt :
    (Position -> FixedPositions -> Position)
    -> (Position -> FixedPositions -> FixedPositions)
    -> (DictFolderFunction Position Rock ( FixedPositions, RockPositions ) -> ( FixedPositions, RockPositions ) -> RockPositions -> ( FixedPositions, RockPositions ))
    -> Map
    -> Map
tilt readNewFixedPosition insertFixedPosition foldFunction map =
    let
        moveRocks ( x, y ) rock ( fixedPoints, currentMap ) =
            if rock == Mobile then
                let
                    newPosition =
                        fixedPoints |> readNewFixedPosition ( x, y )
                in
                ( fixedPoints |> insertFixedPosition newPosition, currentMap |> Dict.insert newPosition Mobile )

            else
                ( fixedPoints |> insertFixedPosition ( x, y ), currentMap |> Dict.insert ( x, y ) Fixed )
    in
    { map | positions = map.positions |> foldFunction moveRocks ( Dict.empty, Dict.empty ) |> Tuple.second }


tiltNorth : Map -> Map
tiltNorth =
    tilt (readFixedY 0 1) insertFixedY Dict.foldl


tiltSouth : Map -> Map
tiltSouth map =
    map |> tilt (readFixedY map.yBoundary -1) insertFixedY Dict.foldr


tiltWest : Map -> Map
tiltWest =
    tilt (readFixedX 0 1) insertFixedX Dict.foldl


tiltEast : Map -> Map
tiltEast map =
    map |> tilt (readFixedX map.xBoundary -1) insertFixedX Dict.foldr


calculateLoad : Map -> Int
calculateLoad { positions, yBoundary } =
    let
        bottomBorder =
            yBoundary + 1

        incrementLoad ( _, y ) rock load =
            if rock == Mobile then
                load + (bottomBorder - y)

            else
                load
    in
    positions |> Dict.foldl incrementLoad 0


draw : Map -> String
draw { positions, xBoundary, yBoundary } =
    let
        getCharacter x y =
            case Dict.get ( x, y ) positions of
                Just Fixed ->
                    "#"

                Just Mobile ->
                    "O"

                Nothing ->
                    "."
    in
    List.range 0 yBoundary
        |> List.map (\y -> List.range 0 xBoundary |> List.foldr (\x acc -> getCharacter x y ++ acc) "")
        |> String.join "\n"


type alias Position =
    ( Int, Int )


type alias Map =
    { positions : RockPositions
    , xBoundary : Int
    , yBoundary : Int
    }


type alias RockPositions =
    Dict Position Rock


type Rock
    = Mobile
    | Fixed


makeMap : String -> Map
makeMap =
    let
        handleSymbol x y symbol ({ positions, xBoundary, yBoundary } as map) =
            let
                newMap =
                    { map | xBoundary = max x xBoundary, yBoundary = max y yBoundary }
            in
            case symbol of
                '#' ->
                    { newMap | positions = positions |> Dict.insert ( x, y ) Fixed }

                'O' ->
                    { newMap | positions = positions |> Dict.insert ( x, y ) Mobile }

                _ ->
                    newMap

        handleRow y row state =
            String.toList row |> List.indexedFoldl (\x symbol -> handleSymbol x y symbol) state
    in
    String.split "\n" >> List.indexedFoldl handleRow { positions = Dict.empty, xBoundary = 0, yBoundary = 0 }
