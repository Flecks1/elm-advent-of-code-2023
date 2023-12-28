module Utils.NonEmptyList exposing (NonEmptyList, fromList, map, toList)


type NonEmptyList a
    = NonEmpty a (List a)


fromList : List a -> Maybe (NonEmptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        a :: rest ->
            Just (NonEmpty a rest)


toList : NonEmptyList a -> List a
toList (NonEmpty head tail) =
    head :: tail


map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map fn (NonEmpty head tail) =
    NonEmpty (fn head) (List.map fn tail)
