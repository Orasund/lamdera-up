module Data.Discussion.Filters exposing (..)

import Dict
import Dict.Extra as Dict


{-|

@docs Filters, create
@docs withTag, withAuthor, favoritedBy
@docs byTag, byAuthor

-}
type Filters
    = Filters
        { tag : Maybe String
        , author : Maybe Int
        , favorited : Maybe Int
        }


create : Filters
create =
    Filters
        { tag = Nothing
        , author = Nothing
        , favorited = Nothing
        }


withTag : String -> Filters -> Filters
withTag tag (Filters filters) =
    Filters { filters | tag = Just tag }


withAuthor : Int -> Filters -> Filters
withAuthor user_id (Filters filters) =
    Filters { filters | author = Just user_id }


favoritedBy : Int -> Filters -> Filters
favoritedBy user_id (Filters filters) =
    Filters { filters | favorited = Just user_id }


byTag mTag discussions =
    case mTag of
        Just tag ->
            discussions |> Dict.filter (\_ a -> a.tags |> List.member tag)

        Nothing ->
            discussions


byAuthor mAuthor users discussions =
    case mAuthor of
        Just id ->
            case users |> Dict.find (\_ u -> u.id == id) |> Maybe.map Tuple.second of
                Just user ->
                    discussions |> Dict.filter (\_ a -> a.userId == user.id)

                Nothing ->
                    discussions

        Nothing ->
            discussions


byFavorite mId users discussions =
    case mId of
        Just id ->
            case users |> Dict.find (\_ u -> u.id == id) |> Maybe.map Tuple.second of
                Just user ->
                    discussions |> Dict.filter (\slug _ -> List.member slug user.favorites)

                Nothing ->
                    discussions

        Nothing ->
            discussions
