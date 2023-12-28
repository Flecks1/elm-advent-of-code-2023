module Utils.Html.Attributes exposing (bemConditional, when)

import Html exposing (Attribute)
import Html.Attributes as Attrs
import Json.Encode as Encode


when : Bool -> Attribute msg -> Attribute msg
when pred attr =
    if pred then
        attr

    else
        Attrs.property "" Encode.null


bemConditional : String -> List ( String, Bool ) -> Attribute msg
bemConditional base modifiers =
    modifiers
        |> List.filterMap
            (\( modifier, isActive ) ->
                if isActive then
                    Just (base ++ "--" ++ modifier)

                else
                    Nothing
            )
        |> (::) base
        |> String.join " "
        |> Attrs.class
