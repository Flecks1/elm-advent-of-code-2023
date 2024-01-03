module Utils.Various exposing (iff, isZero, sign)


iff : Bool -> a -> a -> a
iff pred ifTrue ifFalse =
    if pred then
        ifTrue

    else
        ifFalse


sign : number -> number
sign n =
    n * abs (n ^ -1)


isZero : number -> Bool
isZero n =
    n == n - n
