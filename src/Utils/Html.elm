module Utils.Html exposing (fromMaybe, none)

import Html exposing (Html)
import Maybe.Extra as Maybe


none : Html msg
none =
    Html.text ""


fromMaybe : (a -> Html msg) -> Maybe a -> Html msg
fromMaybe fn =
    Maybe.unwrap none fn
