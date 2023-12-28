module Utils.Various exposing (iff, sign)


iff : Bool -> a -> a -> a
iff pred ifTrue ifFalse =
    if pred then
        ifTrue

    else
        ifFalse


sign : number -> number
sign n =
    n * abs (n ^ -1)
