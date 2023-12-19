module Days.Day10 exposing (first, second)

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Puzzle
import Set exposing (Set)
import Utils.Various exposing (iff)


first : Puzzle.Solution
first input =
    let
        ( map, startTile ) =
            makeMap input

        getFurthestPositionInLoop steps ( state1, state2 ) =
            if state1.currentCoordinates /= state2.currentCoordinates then
                case Result.map2 Tuple.pair (goThroughPipe map state1) (goThroughPipe map state2) of
                    Ok ( newState1, newState2 ) ->
                        getFurthestPositionInLoop (steps + 1) ( newState1, newState2 )

                    Err err ->
                        Err err

            else
                Ok (String.fromInt steps)
    in
    getStartingStates map startTile
        |> Result.andThen (getFurthestPositionInLoop 1)


second : Puzzle.Solution
second input =
    let
        ( map, startTile ) =
            makeMap input

        getTilesInLoopThenFindArea tilesInLoop ( state1, state2 ) =
            if state1.currentCoordinates /= state2.currentCoordinates then
                case Result.map2 Tuple.pair (goThroughPipe map state1) (goThroughPipe map state2) of
                    Ok ( newState1, newState2 ) ->
                        getTilesInLoopThenFindArea (tilesInLoop |> Set.insert newState1.currentCoordinates |> Set.insert newState2.currentCoordinates) ( newState1, newState2 )

                    Err err ->
                        Err err

            else
                findLoopArea (Set.insert state1.currentCoordinates tilesInLoop) (Dict.insert startTile (getTileType map startTile) map)
    in
    getStartingStates map startTile
        |> Result.andThen (\( state1, state2 ) -> getTilesInLoopThenFindArea (Set.fromList [ startTile, state1.currentCoordinates, state2.currentCoordinates ]) ( state1, state2 ))


type alias State =
    { directionWeCameFrom : Direction, currentCoordinates : Coordinates }


goThroughPipe : Dict Coordinates Tile -> State -> Result String State
goThroughPipe map { directionWeCameFrom, currentCoordinates } =
    Dict.get currentCoordinates map
        |> Result.fromMaybe ("We've somehow hit these invalid coordinates: " ++ Debug.toString currentCoordinates)
        |> Result.andThen
            (\tile ->
                case tile of
                    Pipe d1 d2 ->
                        if d1 == directionWeCameFrom then
                            Ok (State (oppositeOf d2) (goDirection d2 currentCoordinates))

                        else if d2 == directionWeCameFrom then
                            Ok (State (oppositeOf d1) (goDirection d1 currentCoordinates))

                        else
                            Err ("We've somehow hit a pipe we should not have been able to go to on these coordinates: " ++ Debug.toString currentCoordinates)

                    _ ->
                        Err ("We've somehow hit a tile that was not even a pipe on these coordinates: " ++ Debug.toString currentCoordinates)
            )


getValidDirectionsFromTile : Dict Coordinates Tile -> Coordinates -> List Direction
getValidDirectionsFromTile map startTile =
    let
        tileCanBeReachedFromDirection direction tile =
            case tile of
                Pipe d1 d2 ->
                    d1 == oppositeOf direction || d2 == oppositeOf direction

                _ ->
                    False

        isDirectionValid direction =
            Dict.get (goDirection direction startTile) map
                |> Maybe.unwrap False (tileCanBeReachedFromDirection direction)
    in
    -- Order matters here if we want getTileType to generate the start tile real pipe the same way we parse them for the input map
    [ north, south, east, west ]
        |> List.filter isDirectionValid


getTileType : Dict Coordinates Tile -> Coordinates -> Tile
getTileType map startTile =
    case getValidDirectionsFromTile map startTile of
        [ direction1, direction2 ] ->
            Pipe direction1 direction2

        _ ->
            Ground


getStartingStates : Dict Coordinates Tile -> Coordinates -> Result String ( State, State )
getStartingStates map startTile =
    let
        validStartingPositions =
            getValidDirectionsFromTile map startTile
                |> List.map (\direction -> { directionWeCameFrom = oppositeOf direction, currentCoordinates = goDirection direction startTile })
    in
    case validStartingPositions of
        [ position1, position2 ] ->
            Ok ( position1, position2 )

        _ ->
            Err "The starting position is not valid and has either too many or too little connecting pipes"


findLoopArea : Set Coordinates -> Dict Coordinates Tile -> Result String String
findLoopArea tilesInLoop map =
    let
        isOdd n =
            not (modBy 2 n == 0)

        isStartingElbow d1 d2 =
            -- We always put the north/south direction in the first position in elbows so we simplify the logic here by not checking any other configuration
            d2 == east && (d1 == south || d1 == north)

        isEndingElbow d1 d2 =
            d2 == west && (d1 == south || d1 == north)

        findElbowEnding startingElbowDirection ({ currentX, currentY, intersections } as state) =
            case Dict.get ( currentX, currentY ) map of
                Just (Pipe d1 d2) ->
                    if isEndingElbow d1 d2 then
                        if d1 == oppositeOf startingElbowDirection then
                            Ok { state | intersections = intersections + 1 }

                        else
                            Ok state

                    else
                        findElbowEnding startingElbowDirection { state | currentX = currentX + 1 }

                _ ->
                    Err ("We somehow found an elbow that was part of the loop and led to a tile that is not a pipe, this shouldn't happen! Coordinates :" ++ Debug.toString ( currentX, currentY ))

        castRays ({ area, currentX, currentY, intersections } as state) =
            case Dict.get ( currentX, currentY ) map of
                Just tile ->
                    if not (Set.member ( currentX, currentY ) tilesInLoop) then
                        castRays { state | currentX = currentX + 1, area = iff (isOdd intersections) (area + 1) area }

                    else
                        case tile of
                            Pipe d1 d2 ->
                                if isStartingElbow d1 d2 then
                                    case findElbowEnding d1 { state | currentX = currentX + 1 } of
                                        Ok newState ->
                                            castRays { newState | currentX = newState.currentX + 1 }

                                        Err err ->
                                            Err err

                                else
                                    -- Ending elbows and horizontal pipes are handled by the rest of the logic, so we know we encountered a vertical pipe
                                    castRays { state | currentX = currentX + 1, intersections = intersections + 1 }

                            _ ->
                                Err ("We somehow found a tile that is in the loop which isn't a pipe during the raycasting process, this shouldn't happen! Coordinates" ++ Debug.toString ( currentX, currentY ))

                Nothing ->
                    -- We reached the bottom of the map
                    if currentX == 0 then
                        area |> String.fromInt |> Ok
                        -- We reached the right boundary of the map

                    else
                        -- Cast a new ray on the next line
                        castRays { state | currentX = 0, currentY = currentY + 1, intersections = 0 }
    in
    castRays { area = 0, currentX = 0, currentY = 0, intersections = 0 }


oppositeOf : Direction -> Direction
oppositeOf ( x, y ) =
    ( -x, -y )


goDirection : Direction -> Coordinates -> Coordinates
goDirection ( x_, y_ ) ( x, y ) =
    ( x_ + x, y_ + y )


type alias Coordinates =
    ( Int, Int )


type alias Direction =
    ( Int, Int )


type alias Steps =
    Int


north : Direction
north =
    ( 0, -1 )


east : Direction
east =
    ( 1, 0 )


south : Direction
south =
    ( 0, 1 )


west : Direction
west =
    ( -1, 0 )


type Tile
    = Start
    | Pipe Direction Direction
    | Ground


toTile : Char -> Tile
toTile symbol =
    case symbol of
        '|' ->
            Pipe north south

        '-' ->
            Pipe east west

        'L' ->
            Pipe north east

        'J' ->
            Pipe north west

        '7' ->
            Pipe south west

        'F' ->
            Pipe south east

        'S' ->
            Start

        _ ->
            Ground



-- Parsing


makeMap : String -> ( Dict Coordinates Tile, Coordinates )
makeMap =
    let
        handleSymbol x y symbol ( dict, position ) =
            let
                tile =
                    toTile symbol
            in
            ( dict |> Dict.insert ( x, y ) tile
            , if tile == Start then
                ( x, y )

              else
                position
            )

        handleRow y row state =
            String.toList row |> List.indexedFoldl (\x symbol -> handleSymbol x y symbol) state
    in
    String.split "\n" >> List.indexedFoldl handleRow ( Dict.empty, ( 0, 0 ) )
