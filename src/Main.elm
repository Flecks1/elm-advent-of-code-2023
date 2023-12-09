module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra as List
import Maybe.Extra as Maybe
import Puzzle exposing (Puzzle)
import Result.Extra as Result
import Utils.Html as Html
import Utils.Html.Events as Events



-- Model and init


puzzles : List Puzzle
puzzles =
    [ { identifier = "test-success", label = "Test success", solution = \_ -> Ok "You did it!" }
    , { identifier = "test-failure", label = "Test failure", solution = \_ -> Err "Lol you suck!" }
    ]


type alias Model =
    { input : String
    , selected : Maybe Puzzle
    , output : Maybe (Result String String)
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" (List.head puzzles) Nothing
    , Cmd.none
    )



-- Messages and update


type Msg
    = InputText String
    | SelectPuzzle String
    | SubmitInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText txt ->
            ( { model | input = txt }, Cmd.none )

        SelectPuzzle identifier ->
            ( { model
                | selected = puzzles |> List.find (.identifier >> (==) identifier)
                , output = Nothing
              }
            , Cmd.none
            )

        SubmitInput ->
            case model.selected of
                Just { solution } ->
                    ( { model | output = Just (solution model.input) }, Cmd.none )

                Nothing ->
                    ( { model | output = Just (Err "No puzzle selected") }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        selectedPuzzleIdentifier =
            model.selected |> Maybe.map .identifier

        puzzleOption puzzle =
            Html.option
                [ Attrs.value puzzle.identifier, Attrs.selected (Just puzzle.identifier == selectedPuzzleIdentifier) ]
                [ Html.text puzzle.identifier ]

        puzzleSelector =
            Html.div [ Attrs.class "form-floating mb-3" ]
                [ puzzles
                    |> List.map puzzleOption
                    |> Html.select [ Attrs.class "form-select", Events.onChange SelectPuzzle, Attrs.name "puzzle-selector" ]
                , Html.label [ Attrs.for "puzzle-selector" ] [ Html.text "Select a puzzle" ]
                ]

        inputField =
            Html.div [ Attrs.class "form-floating mb-3" ]
                [ Html.textarea [ Attrs.name "input", Attrs.class "form-control", Attrs.rows 8, Attrs.cols 40, Events.onInput InputText ]
                    [ Html.text model.input ]
                , Html.label [ Attrs.for "input" ] [ Html.text "Input" ]
                ]

        submitButton =
            Html.button [ Attrs.class "btn btn-primary mb-5", Events.onClick SubmitInput ]
                [ Html.text "Submit" ]

        outputDisplay result =
            Html.div [ Attrs.class "alert", Attrs.classList [ ( "alert-danger", Result.isErr result ), ( "alert-success", Result.isOk result ) ] ]
                [ result |> Result.unpack identity identity |> Html.text ]
    in
    Html.div [ Attrs.class "container mt-5" ]
        [ puzzleSelector
        , inputField
        , submitButton
        , model.output |> Html.fromMaybe outputDisplay
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
