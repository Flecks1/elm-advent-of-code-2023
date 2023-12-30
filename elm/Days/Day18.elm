module Days.Day18 exposing (main)

import Browser
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
import Utils.Cmd as Cmd
import Utils.Direction as Direction exposing (Direction)
import Utils.Html as Html
import Utils.Html.Attributes as Attrs
import Utils.Html.Events as Events
import Utils.String as String



-- Model and init


type Model
    = WaitingForInput
    | GotInput { map : Map, boundaries : Boundaries }
    | ResolvingPartOne PartOneModel
    | ResolvingPartTwo PartTwoModel


type alias PartOneModel =
    { map : Map
    , boundaries : Boundaries
    , rayIntersections : Int
    , rayPosition : Position
    , lastElbowDirection : Maybe Direction
    , answer : Maybe String
    }


type alias PartTwoModel =
    ()


type alias Map =
    Dict Position Cell


type Cell
    = Trench HexCode
    | InteriorArea


type alias HexCode =
    String


type alias Position =
    ( Int, Int )


type alias Boundaries =
    ( Int, Int )


init : ( Model, Cmd Msg )
init =
    ( WaitingForInput
    , Cmd.none
    )



-- Messages and update


type Msg
    = GotFile File
    | NewInputParsed Map
    | RunPartTwo
    | RunPartOne


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile file ->
            ( model, File.toString file |> Task.map fromInput |> Task.perform NewInputParsed )

        NewInputParsed map ->
            ( GotInput { map = map, boundaries = getBoundaries map }
            , Cmd.none
            )

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

        RunPartOne ->
            case model of
                GotInput { map, boundaries } ->
                    initiatePartOne map boundaries

                -- ResolvingPartTwo { map, boundaries } ->
                --     initiatePartOne map boundaries
                ResolvingPartOne subModel ->
                    updatePartOne subModel

                _ ->
                    ( model, Cmd.none )


initiatePartOne : Map -> Boundaries -> ( Model, Cmd Msg )
initiatePartOne map boundaries =
    PartOneModel map boundaries 0 ( -1, 0 ) Nothing Nothing
        |> updatePartOne


updatePartOne : PartOneModel -> ( Model, Cmd Msg )
updatePartOne partOneModel =
    let
        ({ boundaries, map, rayPosition } as newPartOneModel) =
            partOneLoop partOneModel
    in
    -- We've reached the end of the map and should have found the correct answer already
    if Tuple.second rayPosition > Tuple.second boundaries then
        ( ResolvingPartOne { newPartOneModel | answer = Dict.size map |> String.fromInt |> Just, rayPosition = boundaries |> Direction.apply Direction.right }
        , Cmd.none
        )

    else
        ( ResolvingPartOne newPartOneModel
        , Cmd.sendBackMsg 100 RunPartOne
        )


partOneLoop : PartOneModel -> PartOneModel
partOneLoop ({ map, boundaries, rayIntersections, lastElbowDirection } as partOneModel) =
    let
        new =
            { partOneModel | rayPosition = partOneModel.rayPosition |> Direction.apply Direction.right }

        ( _, y ) =
            new.rayPosition

        isTrench cellContent =
            case cellContent of
                Trench _ ->
                    True

                _ ->
                    False

        ( isTargetTrench, isUpTrench, isDownTrench ) =
            ( map |> Dict.get new.rayPosition |> Maybe.unwrap False isTrench
            , map |> Dict.get (new.rayPosition |> Direction.apply Direction.up) |> Maybe.unwrap False isTrench
            , map |> Dict.get (new.rayPosition |> Direction.apply Direction.down) |> Maybe.unwrap False isTrench
            )

        handleElbow direction =
            if lastElbowDirection == Just (Direction.inverse direction) then
                { new | rayIntersections = rayIntersections + 1, lastElbowDirection = Nothing }

            else if lastElbowDirection == Just direction then
                { new | lastElbowDirection = Nothing }

            else
                { new | lastElbowDirection = Just direction }
    in
    case ( isTargetTrench, isUpTrench, isDownTrench ) of
        -- This is just straight piece of trench, consider it an intersection
        ( True, True, True ) ->
            { new | rayIntersections = rayIntersections + 1 }

        -- Up facing elbow
        ( True, True, False ) ->
            handleElbow Direction.up

        -- Down facing elbow
        ( True, False, True ) ->
            handleElbow Direction.down

        -- We're not on any trench cell, add this node to the interior area or leave it as is
        ( False, _, _ ) ->
            -- If the amount of intersections is odd, we're inside the trench
            if modBy 2 rayIntersections == 1 then
                partOneLoop { new | map = map |> Dict.insert new.rayPosition InteriorArea }

            else if Tuple.first new.rayPosition > Tuple.first boundaries then
                -- If we've moved passed the horizontal boundary, reset the ray to the start of the next line
                { new | rayIntersections = 0, rayPosition = ( -1, y + 1 ) }

            else
                -- Loop while we're outside the shape to make the process faster
                partOneLoop new

        -- We're either travelling along a line or not interested in this update. Either way, do nothing and just loop back
        _ ->
            partOneLoop new


initiatePartTwo : Map -> Boundaries -> ( Model, Cmd Msg )
initiatePartTwo map boundaries =
    Debug.todo "TODO"


updatePartTwo : PartTwoModel -> ( Model, Cmd Msg )
updatePartTwo partTwoModel =
    Debug.todo "TODO"



-- View


mapDisplay : { map : Map, xBoundary : Int, yBoundary : Int, rayPosition : Position } -> Html Msg
mapDisplay { map, xBoundary, yBoundary, rayPosition } =
    let
        xRange =
            List.range 0 xBoundary

        displayActiveCellOrNot cellPosition txt otherDisplay =
            if rayPosition == cellPosition then
                Html.span [ Attrs.class "active-map-cell" ] [ Html.text txt ]

            else
                otherDisplay ()

        makeCell cellPosition maybeContent =
            case maybeContent of
                Just (Trench color) ->
                    displayActiveCellOrNot cellPosition "#" <|
                        \_ -> Html.span [ Attrs.style "color" color ] [ Html.text "#" ]

                Just InteriorArea ->
                    displayActiveCellOrNot cellPosition "#" <|
                        \_ -> Html.text "#"

                Nothing ->
                    displayActiveCellOrNot cellPosition "·" <|
                        \_ -> Html.text "·"

        makeRow y =
            xRange
                |> List.map (\x -> Dict.get ( x, y ) map |> makeCell ( x, y ))
                |> Html.div [ Attrs.class "map-row" ]
    in
    List.range 0 yBoundary
        |> List.map makeRow
        |> Html.div [ Attrs.class "map mt-3" ]


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

                -- ResolvingPartTwo { answer } ->
                --     Maybe.isNothing answer
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

                -- ResolvingPartTwo { answer } ->
                --     makeAnswerDisplay answer
                _ ->
                    Html.none

        maybeMapSettings =
            case model of
                GotInput { boundaries, map } ->
                    Just { map = map, xBoundary = Tuple.first boundaries, yBoundary = Tuple.second boundaries, rayPosition = ( 1, 1 ) }

                ResolvingPartOne { boundaries, map, rayPosition } ->
                    Just { map = map, xBoundary = Tuple.first boundaries, yBoundary = Tuple.second boundaries, rayPosition = rayPosition }

                -- ResolvingPartTwo { boundaries, map } ->
                --     Just { map = map, xBoundary = Tuple.first boundaries, yBoundary = Tuple.second boundaries }
                _ ->
                    Nothing
    in
    Html.div [ Attrs.class "container-fluid pt-3" ]
        [ Html.div [ Attrs.class "row" ]
            [ Html.div [ Attrs.class "col-md-4" ] [ fileSelector ]
            , Html.div [ Attrs.class "col-md-6" ] [ runPartOneButton, runPartTwoButton, answerDisplay ]
            ]
        , maybeMapSettings |> Html.fromMaybe mapDisplay
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


getBoundaries : Map -> Boundaries
getBoundaries =
    Dict.foldl (\( x, y ) _ ( xBoundary, yBoundary ) -> ( max x xBoundary, max y yBoundary )) ( 0, 0 )



-- Parsing


translateMapOrigin : Map -> Map
translateMapOrigin map =
    let
        maxInt =
            2147483647

        ( originX, originY ) =
            map |> Dict.foldl (\( x, y ) _ ( minimumX, minimumY ) -> ( min x minimumX, min y minimumY )) ( maxInt, maxInt )
    in
    map |> Dict.foldl (\( x, y ) -> Dict.insert ( x - originX, y - originY )) Dict.empty


fromInput : String -> Map
fromInput =
    let
        interpretDirection str =
            case str of
                "L" ->
                    Direction.left

                "U" ->
                    Direction.up

                "R" ->
                    Direction.right

                _ ->
                    Direction.down

        digTrench ( currentPosition, map ) ( direction, steps, color ) =
            if steps /= 0 then
                let
                    newPosition =
                        currentPosition |> Direction.apply direction
                in
                digTrench ( newPosition, map |> Dict.insert newPosition (Trench color) ) ( direction, steps - 1, color )

            else
                ( currentPosition, map )

        decomposeRow row =
            case String.split " " row of
                [ direction, steps, color ] ->
                    Just ( interpretDirection direction, String.toInt steps |> Maybe.withDefault 1, String.slice 1 -1 color )

                _ ->
                    Nothing

        handleRow row state =
            decomposeRow row |> Maybe.unwrap state (digTrench state)
    in
    String.split "\n"
        >> List.foldl handleRow ( ( 0, 0 ), Dict.empty )
        >> Tuple.second
        >> translateMapOrigin
