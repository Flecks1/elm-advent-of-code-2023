module Days.Day16 exposing (main)

import Array.Extra as Array
import Browser
import Dict exposing (Dict)
import File exposing (File)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Keyed
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Task
import Utils.Html as Html
import Utils.Html.Attributes as Attrs
import Utils.Html.Events as Events



-- Model and init


type Model
    = WaitingForInput { error : Maybe String }
    | GotInput { map : Map, boundaries : ( X, Y ) }
    | ResolvingPartOne PartOneModel


type alias PartOneModel =
    { map : Map, boundaries : ( X, Y ), activeBeams : List Beam, answer : Maybe String }


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
    ( WaitingForInput { error = Nothing }
    , Cmd.none
    )



-- Messages and update


type Msg
    = GotFile File
    | NewInputParsed Map
    | RunPartOne


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        loopPartOne =
            Process.sleep 42 |> Task.perform (\_ -> RunPartOne)
    in
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
                    ( ResolvingPartOne { map = map, boundaries = boundaries, activeBeams = [ Beam ( -1, 0 ) ( 1, 0 ) ], answer = Nothing }
                    , loopPartOne
                    )

                ResolvingPartOne ({ activeBeams } as subModel) ->
                    if not (List.isEmpty activeBeams) then
                        ( ResolvingPartOne (updatePartOne subModel)
                        , loopPartOne
                        )

                    else
                        ( ResolvingPartOne { subModel | answer = Just (getPartOneAnswer subModel.map) }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )


updatePartOne : PartOneModel -> PartOneModel
updatePartOne { map, boundaries, activeBeams, answer } =
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
    activeBeams |> List.foldl beamsProgress { map = map, boundaries = boundaries, activeBeams = [], answer = answer }


getPartOneAnswer : Map -> String
getPartOneAnswer =
    let
        increment _ { beams } acc =
            if alreadyHasBeamsComingFromAnyDirection beams then
                acc + 1

            else
                acc
    in
    Dict.foldl increment 0 >> String.fromInt


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


mapDisplay : ( X, Y ) -> Map -> Html msg
mapDisplay ( xBoundary, yBoundary ) map =
    let
        mapCell ( x, y ) cell =
            ( "cell-" ++ String.fromInt x ++ "-" ++ String.fromInt y
            , Html.td
                [ Attrs.bemConditional "mirror-map-cell"
                    [ ( "mirror-left", cell.content == MirrorLeft )
                    , ( "mirror-right", cell.content == MirrorRight )
                    , ( "splitter-horizontal", cell.content == SplitterHorizontal )
                    , ( "splitter-vertical", cell.content == SplitterVertical )
                    , ( "beam-up", cell.beams.up )
                    , ( "beam-down", cell.beams.down )
                    , ( "beam-left", cell.beams.left )
                    , ( "beam-right", cell.beams.right )
                    ]
                ]
                [ Html.span [ Attrs.class "cell-beams" ] []
                , Html.span [ Attrs.class "cell-content" ] []
                ]
            )

        defaultCell =
            Cell Empty (BeamDirections False False False False)

        mapRow y =
            ( "row-" ++ String.fromInt y
            , List.range 0 xBoundary
                |> List.map (\x -> Dict.get ( x, y ) map |> Maybe.withDefault defaultCell |> mapCell ( x, y ))
                |> Html.Keyed.node "tr" []
            )
    in
    List.range 0 yBoundary
        |> List.map mapRow
        |> Html.Keyed.node "table" [ Attrs.class "mirror-map" ]
        |> List.singleton
        |> Html.div [ Attrs.class "mt-3 mb-3" ]


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

        runPartOneButton =
            Html.button [ Attrs.class "btn btn-primary ml-5", Events.onClick RunPartOne ]
                [ Html.text "Run part one" ]

        maybeAnswer =
            case model of
                ResolvingPartOne { answer } ->
                    answer

                _ ->
                    Nothing

        maybeBoundariesAndMap =
            case model of
                GotInput { boundaries, map } ->
                    Just ( boundaries, map )

                ResolvingPartOne { boundaries, map } ->
                    Just ( boundaries, map )

                _ ->
                    Nothing
    in
    Html.div [ Attrs.class "container-fluid pt-3" ]
        [ Html.div [ Attrs.class "row" ]
            [ Html.div [ Attrs.class "col-md-4" ] [ fileSelector ]
            , Html.div [ Attrs.class "col-md-4" ] [ runPartOneButton, maybeAnswer |> Html.fromMaybe Html.text ]
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
