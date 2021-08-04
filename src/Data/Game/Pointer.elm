module Data.Game.Pointer exposing (Pointer, fromInt, get, getFromArray, update)

import Array exposing (Array)
import Dict exposing (Dict)


type Pointer a
    = Pointer Int


fromInt : Int -> Pointer a
fromInt int =
    Pointer int


getFromArray : Pointer a -> Array a -> Maybe a
getFromArray (Pointer int) =
    Array.get int


get : Pointer a -> Dict Int a -> Maybe a
get (Pointer int) =
    Dict.get int


update : Pointer a -> (Maybe a -> Maybe a) -> Dict Int a -> Dict Int a
update (Pointer int) =
    Dict.update int
