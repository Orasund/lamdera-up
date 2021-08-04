module Data.Response exposing (Response(..), map, toMaybe)


type Response value
    = NotAsked
    | Loading
    | Failure (List String)
    | Success value


map : (a -> b) -> Response a -> Response b
map fn data =
    case data of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure reason ->
            Failure reason

        Success value ->
            Success (fn value)


toMaybe : Response value -> Maybe value
toMaybe data =
    case data of
        Success value ->
            Just value

        _ ->
            Nothing
