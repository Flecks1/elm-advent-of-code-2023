module Main exposing (main)

import Browser
import Days.Day1
import Days.Day10
import Days.Day11
import Days.Day12
import Days.Day13
import Days.Day14
import Days.Day15
import Days.Day19
import Days.Day2
import Days.Day20
import Days.Day21
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6
import Days.Day7
import Days.Day8
import Days.Day9
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
    [ { identifier = "day1-1", label = "Day 1", solution = Days.Day1.first }
    , { identifier = "day1-2", label = "Day 1 (Part Two)", solution = Days.Day1.second }
    , { identifier = "day2-1", label = "Day 2", solution = Days.Day2.first }
    , { identifier = "day2-2", label = "Day 2 (Part Two)", solution = Days.Day2.second }
    , { identifier = "day3-1", label = "Day 3", solution = Days.Day3.first }
    , { identifier = "day3-2", label = "Day 3 (Part Two)", solution = Days.Day3.second }
    , { identifier = "day4-1", label = "Day 4", solution = Days.Day4.first }
    , { identifier = "day4-2", label = "Day 4 (Part Two)", solution = Days.Day4.second }
    , { identifier = "day5-1", label = "Day 5", solution = Days.Day5.first }
    , { identifier = "day5-2", label = "Day 5 (Part Two) ⚠ Takes a stupidly long time to run", solution = Days.Day5.second }
    , { identifier = "day6-1", label = "Day 6", solution = Days.Day6.first }
    , { identifier = "day6-2", label = "Day 6 (Part Two)", solution = Days.Day6.second }
    , { identifier = "day7-1", label = "Day 7", solution = Days.Day7.first }
    , { identifier = "day7-2", label = "Day 7 (Part Two)", solution = Days.Day7.second }
    , { identifier = "day8-1", label = "Day 8", solution = Days.Day8.first }
    , { identifier = "day8-2", label = "Day 8 (Part Two)", solution = Days.Day8.second }
    , { identifier = "day9-1", label = "Day 9", solution = Days.Day9.first }
    , { identifier = "day9-2", label = "Day 9 (Part Two)", solution = Days.Day9.second }
    , { identifier = "day10-1", label = "Day 10", solution = Days.Day10.first }
    , { identifier = "day10-2", label = "Day 10 (Part Two)", solution = Days.Day10.second }
    , { identifier = "day11-1", label = "Day 11", solution = Days.Day11.first }
    , { identifier = "day11-2", label = "Day 11 (Part Two)", solution = Days.Day11.second }
    , { identifier = "day12-1", label = "Day 12", solution = Days.Day12.first }
    , { identifier = "day13-1", label = "Day 13", solution = Days.Day13.first }
    , { identifier = "day13-2", label = "Day 13 (Part Two)", solution = Days.Day13.second }
    , { identifier = "day14-1", label = "Day 14", solution = Days.Day14.first }
    , { identifier = "day14-2", label = "Day 14 (Part Two)", solution = Days.Day14.second }
    , { identifier = "day15-1", label = "Day 15", solution = Days.Day15.first }
    , { identifier = "day15-2", label = "Day 15 (Part Two)", solution = Days.Day15.second }
    , { identifier = "day19-1", label = "Day 19", solution = Days.Day19.first }
    , { identifier = "day19-2", label = "Day 19 (Part Two)", solution = Days.Day19.second }
    , { identifier = "day20-1", label = "Day 20", solution = Days.Day20.first }
    , { identifier = "day20-2", label = "Day 20 (Part Two)", solution = Days.Day20.second }
    , { identifier = "day21-1", label = "Day 21", solution = Days.Day21.first }
    , { identifier = "day21-2", label = "Day 21 (Part Two)", solution = Days.Day21.second }
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
                [ Html.text puzzle.label ]

        puzzleSelector =
            Html.div [ Attrs.class "form-floating mb-3" ]
                [ puzzles
                    |> List.map puzzleOption
                    |> Html.select [ Attrs.class "form-select", Events.onChange SelectPuzzle, Attrs.name "puzzle-selector" ]
                , Html.label [ Attrs.for "puzzle-selector" ] [ Html.text "Select a puzzle" ]
                ]

        inputField =
            Html.div [ Attrs.class "form-floating mb-3" ]
                [ Html.textarea [ Attrs.name "input", Attrs.class "form-control", Attrs.style "height" "400px", Attrs.style "font-family" "monospace", Events.onInput InputText ]
                    [ Html.text model.input ]
                , Html.label [ Attrs.for "input" ] [ Html.text "Input" ]
                ]

        submitButton =
            Html.button [ Attrs.class "btn btn-primary mb-5", Events.onClick SubmitInput ]
                [ Html.text "Submit" ]

        outputDisplay result =
            Html.div [ Attrs.class "alert", Attrs.classList [ ( "alert-danger", Result.isErr result ), ( "alert-success", Result.isOk result ) ] ]
                [ Html.pre [] [ result |> Result.unpack identity identity |> Html.text ] ]
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
