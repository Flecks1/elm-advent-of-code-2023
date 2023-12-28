module Days.Day16 exposing (main)

import Browser
import Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas
import Canvas.Settings.Line as Canvas
import Color exposing (Color)
import Dict exposing (Dict)
import File exposing (File)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Task
import Utils.Html as Html
import Utils.Html.Attributes as Attrs
import Utils.Html.Events as Events
import Utils.Various exposing (iff)



-- Model and init


type Model
    = WaitingForInput
    | GotInput { map : Map, boundaries : ( X, Y ) }
    | ResolvingPartOne { map : Map, boundaries : ( X, Y ), activeBeams : List Beam, answer : Maybe String }
    | ResolvingPartTwo { map : Map, boundaries : ( X, Y ), activeBeams : List Beam, maximum : Int, startingBeams : List Beam, answer : Maybe String }


type alias Map =
    Dict Position Cell


type alias X =
    Int


type alias Y =
    Int


type alias Beam =
    { position : Position, direction : Direction }


type alias Position =
    ( X, Y )


type alias Direction =
    ( X, Y )


type alias Cell =
    { content : CellContent, beams : BeamDirections }


type CellContent
    = Empty
    | MirrorLeft
    | MirrorRight
    | SplitterHorizontal
    | SplitterVertical


type alias BeamDirections =
    { up : Bool, right : Bool, down : Bool, left : Bool }


init : ( Model, Cmd Msg )
init =
    ( WaitingForInput
    , Cmd.none
    )



-- Messages and update


type Msg
    = GotFile File
    | NewInputParsed Map
    | RunPartOne
    | RunPartTwo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile file ->
            ( model, File.toString file |> Task.map fromInput |> Task.perform NewInputParsed )

        NewInputParsed map ->
            ( GotInput { map = map, boundaries = getBoundaries map }
            , Cmd.none
            )

        RunPartOne ->
            case model of
                GotInput { map, boundaries } ->
                    initiatePartOne map boundaries

                ResolvingPartTwo { map, boundaries } ->
                    initiatePartOne map boundaries

                ResolvingPartOne subModel ->
                    updatePartOne subModel

                _ ->
                    ( model, Cmd.none )

        RunPartTwo ->
            case model of
                GotInput { map, boundaries } ->
                    initiatePartTwo map boundaries

                ResolvingPartOne { map, boundaries } ->
                    initiatePartTwo map boundaries

                ResolvingPartTwo subModel ->
                    updatePartTwo subModel

                _ ->
                    ( model, Cmd.none )


delayMsg : Float -> Msg -> Cmd Msg
delayMsg milliseconds msg =
    Process.sleep milliseconds |> Task.perform (\_ -> msg)


initiatePartOne : Map -> ( X, Y ) -> ( Model, Cmd Msg )
initiatePartOne map boundaries =
    ( ResolvingPartOne { map = resetMap map, boundaries = boundaries, activeBeams = [ Beam ( -1, 0 ) ( 1, 0 ) ], answer = Nothing }
    , delayMsg 50 RunPartOne
    )


updatePartOne : { map : Map, boundaries : ( X, Y ), activeBeams : List Beam, answer : Maybe String } -> ( Model, Cmd Msg )
updatePartOne ({ activeBeams, map } as partOneModel) =
    if not (List.isEmpty activeBeams) then
        ( ResolvingPartOne (updateBeams partOneModel)
        , delayMsg 50 RunPartOne
        )

    else
        ( ResolvingPartOne { partOneModel | answer = Just (getAmountOfCellsTouchedByBeam map |> String.fromInt) }
        , Cmd.none
        )


initiatePartTwo : Map -> ( X, Y ) -> ( Model, Cmd Msg )
initiatePartTwo map boundaries =
    updatePartTwo { map = resetMap map, boundaries = boundaries, activeBeams = [], maximum = 0, startingBeams = getStartingBeams boundaries, answer = Nothing }


updatePartTwo : { map : Map, boundaries : ( X, Y ), activeBeams : List Beam, maximum : Int, startingBeams : List Beam, answer : Maybe String } -> ( Model, Cmd Msg )
updatePartTwo partTwoModel =
    let
        calculateResultForStartingBeamAndLoop currentState =
            if not (List.isEmpty currentState.activeBeams) then
                calculateResultForStartingBeamAndLoop (updateBeams currentState)

            else
                ( ResolvingPartTwo { currentState | maximum = max currentState.maximum (getAmountOfCellsTouchedByBeam currentState.map) }
                , delayMsg 100 RunPartTwo
                )
    in
    case partTwoModel.startingBeams of
        startingBeam :: rest ->
            calculateResultForStartingBeamAndLoop { partTwoModel | startingBeams = rest, activeBeams = [ startingBeam ], map = resetMap partTwoModel.map }

        _ ->
            ( ResolvingPartTwo { partTwoModel | answer = Just (String.fromInt partTwoModel.maximum) }
            , Cmd.none
            )


getStartingBeams : ( X, Y ) -> List Beam
getStartingBeams ( xBoundary, yBoundary ) =
    let
        xRange =
            List.range 0 xBoundary

        yRange =
            List.range 0 yBoundary

        xStartingBeams =
            xRange |> List.foldr (\x startingBeams -> Beam ( x, -1 ) ( 0, 1 ) :: Beam ( x, yBoundary + 1 ) ( 0, -1 ) :: startingBeams) []
    in
    yRange |> List.foldr (\y startingBeams -> Beam ( -1, y ) ( 1, 0 ) :: Beam ( xBoundary + 1, y ) ( -1, 0 ) :: startingBeams) xStartingBeams


type alias UpdatableBeamsState r =
    { r | map : Map, activeBeams : List Beam }


updateBeams : UpdatableBeamsState r -> UpdatableBeamsState r
updateBeams ({ activeBeams } as initialState) =
    let
        beamsProgress { position, direction } state =
            let
                newPosition =
                    add direction position
            in
            case state.map |> getCell newPosition of
                Just ({ beams } as cell) ->
                    if beams |> alreadyHasBeamComingFromDirection direction |> not then
                        let
                            ( newCell, newBeams ) =
                                cell |> deviateBeam direction |> Tuple.mapSecond (List.map (Beam newPosition))
                        in
                        { state
                            | map = state.map |> setCell newPosition newCell
                            , activeBeams = newBeams ++ state.activeBeams
                        }

                    else
                        state

                Nothing ->
                    state
    in
    activeBeams |> List.foldl beamsProgress { initialState | activeBeams = [] }


getAmountOfCellsTouchedByBeam : Map -> Int
getAmountOfCellsTouchedByBeam =
    let
        increment _ { beams } acc =
            if alreadyHasBeamsComingFromAnyDirection beams then
                acc + 1

            else
                acc
    in
    Dict.foldl increment 0


deviateBeam : Direction -> Cell -> ( Cell, List Direction )
deviateBeam (( deltaX, deltaY ) as direction) ({ content, beams } as cell) =
    let
        newDirections =
            case content of
                MirrorLeft ->
                    [ ( deltaY, deltaX ) ]

                MirrorRight ->
                    [ ( -deltaY, -deltaX ) ]

                SplitterHorizontal ->
                    if direction == ( 0, -1 ) || direction == ( 0, 1 ) then
                        if not (alreadyHasBeamsComingFromAnyDirection beams) then
                            [ ( -1, 0 ), ( 1, 0 ) ]

                        else
                            []

                    else
                        [ ( deltaX, deltaY ) ]

                SplitterVertical ->
                    if direction == ( -1, 0 ) || direction == ( 1, 0 ) then
                        if not (alreadyHasBeamsComingFromAnyDirection beams) then
                            [ ( 0, -1 ), ( 0, 1 ) ]

                        else
                            []

                    else
                        [ ( deltaX, deltaY ) ]

                Empty ->
                    [ ( deltaX, deltaY ) ]

        newBeams =
            case content of
                MirrorLeft ->
                    if direction == ( 1, 0 ) || direction == ( 0, -1 ) then
                        { beams | left = True, down = True }

                    else
                        { beams | right = True, up = True }

                MirrorRight ->
                    if direction == ( 1, 0 ) || direction == ( 0, 1 ) then
                        { beams | left = True, up = True }

                    else
                        { beams | right = True, down = True }

                SplitterHorizontal ->
                    if direction == ( 0, -1 ) then
                        { beams | left = True, right = True, down = True }

                    else if direction == ( 0, 1 ) then
                        { beams | left = True, right = True, up = True }

                    else
                        { beams | left = True, right = True }

                SplitterVertical ->
                    if direction == ( -1, 0 ) then
                        { beams | up = True, down = True, right = True }

                    else if direction == ( 1, 0 ) then
                        { beams | up = True, down = True, left = True }

                    else
                        { beams | up = True, down = True }

                Empty ->
                    if direction == ( -1, 0 ) || direction == ( 1, 0 ) then
                        { beams | left = True, right = True }

                    else
                        { beams | up = True, down = True }
    in
    ( { cell | beams = newBeams }, newDirections )


resetMap : Map -> Map
resetMap =
    Dict.map (\_ cell -> { cell | beams = BeamDirections False False False False })


getBoundaries : Map -> ( X, Y )
getBoundaries =
    Dict.foldl (\( x, y ) _ ( xBoundary, yBoundary ) -> ( max x xBoundary, max y yBoundary )) ( 0, 0 )


setCell : Position -> Cell -> Map -> Map
setCell position cell =
    Dict.insert position cell


getCell : Position -> Map -> Maybe Cell
getCell =
    Dict.get


add : Direction -> Position -> Position
add ( deltaX, deltaY ) ( x, y ) =
    ( x + deltaX, y + deltaY )


alreadyHasBeamsComingFromAnyDirection : BeamDirections -> Bool
alreadyHasBeamsComingFromAnyDirection { up, right, down, left } =
    up || right || down || left


alreadyHasBeamComingFromDirection : ( X, Y ) -> BeamDirections -> Bool
alreadyHasBeamComingFromDirection direction { up, right, down, left } =
    if direction == ( 1, 0 ) then
        left

    else if direction == ( -1, 0 ) then
        right

    else if direction == ( 0, 1 ) then
        up

    else
        down



-- View


gridBackgroundColor : Color
gridBackgroundColor =
    Color.rgb255 30 33 37


gridSecondaryBackgroundColor : Color
gridSecondaryBackgroundColor =
    Color.rgb255 28 30 34


toCellRenderables : ( X, Y ) -> Cell -> List Canvas.Renderable
toCellRenderables ( x, y ) { content, beams } =
    let
        ( canvasX, canvasY ) =
            ( toFloat x * 17.0, toFloat y * 17.0 )

        verticalBeam =
            if beams.up || beams.down then
                let
                    startingPointY =
                        iff beams.up canvasY (canvasY + 8.0)

                    strokeLength =
                        iff (beams.up && beams.down) 17.0 9.0
                in
                [ Canvas.path ( canvasX + 8.5, startingPointY ) [ Canvas.lineTo ( canvasX + 8.5, startingPointY + strokeLength ) ] ]

            else
                []

        horizontalBeam =
            if beams.left || beams.right then
                let
                    startingPointX =
                        iff beams.left canvasX (canvasX + 8.0)

                    strokeLength =
                        iff (beams.left && beams.right) 17.0 9.0
                in
                [ Canvas.path ( startingPointX, canvasY + 8.5 ) [ Canvas.lineTo ( startingPointX + strokeLength, canvasY + 8.5 ) ] ]

            else
                []

        beamsShape =
            if beams.up || beams.right || beams.down || beams.left then
                [ (horizontalBeam ++ verticalBeam) |> Canvas.shapes [ Canvas.stroke Color.green, Canvas.lineWidth 1.0 ] ]

            else
                []

        makeMirrorShape startingPoint endingPoint =
            [ Canvas.shapes [ Canvas.stroke Color.red, Canvas.lineWidth 2.5 ] [ Canvas.path startingPoint [ Canvas.lineTo endingPoint ] ] ]

        mirrorShape =
            if content == MirrorLeft then
                makeMirrorShape ( canvasX + 3.5, canvasY + 3.5 ) ( canvasX + 13.5, canvasY + 13.5 )

            else if content == MirrorRight then
                makeMirrorShape ( canvasX + 13.5, canvasY + 3.5 ) ( canvasX + 3.5, canvasY + 13.5 )

            else if content == SplitterHorizontal then
                makeMirrorShape ( canvasX + 2.5, canvasY + 8.5 ) ( canvasX + 14.5, canvasY + 8.5 )

            else if content == SplitterVertical then
                makeMirrorShape ( canvasX + 8.5, canvasY + 2.5 ) ( canvasX + 8.5, canvasY + 14.5 )

            else
                []

        -- alternateBackground =
        --     if modBy 2 (x + y) /= 0 then
        --         [ Canvas.shapes [ Canvas.fill gridSecondaryBackgroundColor ]
        --             [ Canvas.rect ( toFloat x * 17.0, toFloat y * 17.0 ) 17.0 17.0 ]
        --         ]
        --     else
        --         []
    in
    List.concat [ {- alternateBackground, -} mirrorShape, beamsShape ]


mapDisplay : ( X, Y ) -> Map -> Html msg
mapDisplay ( xBoundary, yBoundary ) map =
    let
        xRange =
            List.range 0 xBoundary

        defaultCell =
            Cell Empty (BeamDirections False False False False)

        mapRow y =
            xRange |> List.concatMap (\x -> Dict.get ( x, y ) map |> Maybe.withDefault defaultCell |> toCellRenderables ( x, y ))

        ( width, height ) =
            ( toFloat xBoundary * 17.0, toFloat yBoundary * 17.0 )
    in
    Canvas.toHtml ( round width, round height ) [ Attrs.style "display" "block", Attrs.class "mt-3" ] <|
        (Canvas.shapes [ Canvas.fill gridBackgroundColor ] [ Canvas.rect ( 0, 0 ) width height ]
            :: (List.range 0 yBoundary |> List.concatMap mapRow)
        )


view : Model -> Html Msg
view model =
    let
        fileDecoder =
            Decode.at [ "target", "files", "0" ] File.decoder

        fileSelector =
            Html.input
                [ Attrs.type_ "file"
                , Attrs.class "form-control"
                , Events.on "change" (Decode.map GotFile fileDecoder)
                ]
                []

        isRunning =
            case model of
                ResolvingPartOne { answer } ->
                    Maybe.isNothing answer

                ResolvingPartTwo { answer } ->
                    Maybe.isNothing answer

                _ ->
                    False

        runPartOneButton =
            Html.button [ Attrs.class "btn btn-primary ms-5", Events.onClick RunPartOne |> Attrs.when (not isRunning), Attrs.disabled isRunning ]
                [ Html.text "Run part one" ]

        runPartTwoButton =
            Html.button [ Attrs.class "btn btn-primary ms-3", Events.onClick RunPartTwo |> Attrs.when (not isRunning), Attrs.disabled isRunning ]
                [ Html.text "Run part two" ]

        makeAnswerDisplay =
            Html.fromMaybe (\answer -> Html.span [ Attrs.class "ms-5" ] [ Html.text answer ])

        answerDisplay =
            case model of
                ResolvingPartOne { answer } ->
                    makeAnswerDisplay answer

                ResolvingPartTwo { answer } ->
                    makeAnswerDisplay answer

                _ ->
                    Html.none

        maybeBoundariesAndMap =
            case model of
                GotInput { boundaries, map } ->
                    Just ( boundaries, map )

                ResolvingPartOne { boundaries, map } ->
                    Just ( boundaries, map )

                ResolvingPartTwo { boundaries, map } ->
                    Just ( boundaries, map )

                _ ->
                    Nothing
    in
    Html.div [ Attrs.class "container-fluid pt-3" ]
        [ Html.div [ Attrs.class "row" ]
            [ Html.div [ Attrs.class "col-md-4" ] [ fileSelector ]
            , Html.div [ Attrs.class "col-md-6" ] [ runPartOneButton, runPartTwoButton, answerDisplay ]
            ]
        , maybeBoundariesAndMap |> Html.fromMaybe (\( boundaries, map ) -> mapDisplay boundaries map)
        ]



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- Helpers


fromInput : String -> Map
fromInput =
    let
        toContent symbol =
            case symbol of
                '\\' ->
                    MirrorLeft

                '/' ->
                    MirrorRight

                '-' ->
                    SplitterHorizontal

                '|' ->
                    SplitterVertical

                _ ->
                    Empty

        handleSymbol x y symbol =
            { content = toContent symbol, beams = BeamDirections False False False False }
                |> Dict.insert ( x, y )

        handleRow y row state =
            String.toList row |> List.indexedFoldl (\x symbol -> handleSymbol x y symbol) state
    in
    String.split "\u{000D}\n" >> List.indexedFoldl handleRow Dict.empty
