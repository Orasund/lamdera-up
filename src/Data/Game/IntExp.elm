module Data.Game.IntExp exposing (IntExp(..), evaluate)

import Random exposing (Generator)


type IntExp
    = IntValue Int
    | IntRandom IntExp IntExp
    | IntVar


evaluate : IntExp -> Int -> Generator Int
evaluate int var =
    case int of
        IntValue i ->
            Random.constant i

        IntRandom min max ->
            Random.map2
                Tuple.pair
                (evaluate min var)
                (evaluate max var)
                |> Random.andThen (\( a, b ) -> Random.int a b)

        IntVar ->
            Random.constant var
