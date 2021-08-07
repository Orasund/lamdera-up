module Data.Game.IntExp exposing (IntExp(..), evaluate)

import Random exposing (Generator)


type IntExp
    = IntValue Int
    | IntRandom IntExp IntExp


evaluate : IntExp -> Generator Int
evaluate int =
    case int of
        IntValue i ->
            Random.constant i

        IntRandom min max ->
            Random.map2
                Tuple.pair
                (evaluate min)
                (evaluate max)
                |> Random.andThen (\( a, b ) -> Random.int a b)
