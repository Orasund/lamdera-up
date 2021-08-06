module Data.Article.Filters exposing (..)

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


byTag mTag articles =
    case mTag of
        Just tag ->
            articles |> Dict.filter (\_ a -> a.tags |> List.member tag)

        Nothing ->
            articles


byAuthor mAuthor users articles =
    case mAuthor of
        Just id ->
            case users |> Dict.find (\_ u -> u.id == id) |> Maybe.map Tuple.second of
                Just user ->
                    articles |> Dict.filter (\_ a -> a.userId == user.id)

                Nothing ->
                    articles

        Nothing ->
            articles


byFavorite mId users articles =
    case mId of
        Just id ->
            case users |> Dict.find (\_ u -> u.id == id) |> Maybe.map Tuple.second of
                Just user ->
                    articles |> Dict.filter (\slug _ -> List.member slug user.favorites)

                Nothing ->
                    articles

        Nothing ->
            articles
