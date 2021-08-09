module Data.Game.Player exposing (Player, PlayerId(..), getPointer, idsOrderedByPoints, new, toString)

import Array
import Data.Game.Pointer as Pointer exposing (Pointer)
import Dict exposing (Dict)
import List.Extra as List


type alias Player =
    { points : Int
    , tokens : Int
    }


type PlayerId
    = Current
    | Relative Int
    | Highest


toString : PlayerId -> String
toString playerId =
    case playerId of
        Current ->
            "you"

        Relative int ->
            if int == 0 then
                toString Current

            else if int > 0 then
                "the " ++ String.fromInt int ++ ". higher player"

            else
                "the " ++ String.fromInt int ++ ". lower player"

        Highest ->
            "the highest player"


new : Player
new =
    { points = 0
    , tokens = 0
    }


idsOrderedByPoints : Dict Int Player -> List (Pointer Player)
idsOrderedByPoints player =
    player
        |> Dict.toList
        |> List.sortBy (\( _, v ) -> v.points)
        |> List.map (Tuple.first >> Pointer.fromInt)


getPointer :
    { playerId : PlayerId, current : Pointer Player }
    -> List (Pointer Player)
    -> Maybe (Pointer Player)
getPointer args list =
    case args.playerId of
        Current ->
            Just args.current

        Relative int ->
            list
                |> List.findIndex ((==) args.current)
                |> Maybe.andThen
                    (\index ->
                        list
                            |> Array.fromList
                            |> Array.get (index + int)
                    )

        Highest ->
            list
                |> List.last
