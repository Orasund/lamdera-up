module Data.Discussion exposing (..)

{-|

@docs Discussion, Listing, updateDiscussion, itemsPerPage

-}

import Data.Profile exposing (Profile)
import Time


type alias Discussion =
    { slug : Slug
    , title : String
    , description : String
    , body : String
    , tags : List String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
    , author : Profile
    }


type alias DiscussionStore =
    { slug : Slug
    , title : String
    , description : String
    , body : String
    , tags : List String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , userId : Int
    }


type alias Slug =
    String


type alias Listing =
    { discussions : List Discussion
    , page : Int
    , totalPages : Int
    }


updateDiscussion : Discussion -> Listing -> Listing
updateDiscussion discussion listing =
    let
        discussions : List Discussion
        discussions =
            List.map
                (\a ->
                    if a.slug == discussion.slug then
                        discussion

                    else
                        a
                )
                listing.discussions
    in
    { listing | discussions = discussions }



-- INTERNALS


itemsPerPage : Int
itemsPerPage =
    25
