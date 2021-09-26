module Data.Game.Pointer exposing (Pointer, find, fromInt, get, update)

import Array
import Dict exposing (Dict)
import List.Extra as List


type Pointer a
    = Pointer Int


fromInt : Int -> Pointer a
fromInt int =
    Pointer int


find : (a -> Int) -> Pointer a -> List a -> Maybe a
find fun (Pointer int) =
    List.find (fun >> (==) int)


get : Pointer a -> Dict Int a -> Maybe a
get (Pointer int) =
    Dict.get int


update : Pointer a -> (Maybe a -> Maybe a) -> Dict Int a -> Dict Int a
update (Pointer int) =
    Dict.update int
