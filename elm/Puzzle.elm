module Puzzle exposing (Puzzle, Solution)


type alias Solution =
    String -> Result String String


type alias Puzzle =
    { identifier : String
    , label : String
    , solution : Solution
    }
