module TimeDiff exposing (delta, plus)

import Time exposing (..)

delta : Posix -> Posix -> Int
delta subThis fromThat =
  (posixToMillis fromThat) - (posixToMillis subThis)

plus : Int -> Posix -> Posix
plus addThis toThat =
  (posixToMillis toThat) + addThis
    |> millisToPosix
