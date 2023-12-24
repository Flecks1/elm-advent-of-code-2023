module Utils.Html.Attributes exposing (bemConditional)

import Html exposing (Attribute)
import Html.Attributes as Attrs


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
