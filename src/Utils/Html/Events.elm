module Utils.Html.Events exposing (onChange)

import Html exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    Events.on "change" (Events.targetValue |> Decode.map tagger)
