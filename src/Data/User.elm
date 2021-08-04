module Data.User exposing (..)

{-|

@docs User, UserFull, Email

-}

import Data.Article exposing (Slug)
import Data.Profile exposing (Profile)


type alias User =
    { id : Int
    , email : Email
    , username : String
    , bio : Maybe String
    , points : Int
    , tokens : Int
    }


type alias UserFull =
    { id : Int
    , email : Email
    , username : String
    , bio : Maybe String
    , password : String
    , favorites : List Slug
    , following : List UserId
    , points : Int
    , tokens : Int
    }


type alias UserId =
    Int


toUser : UserFull -> User
toUser u =
    { id = u.id
    , email = u.email
    , username = u.username
    , bio = u.bio
    , points = u.points
    , tokens = u.tokens
    }


toProfile : UserFull -> Profile
toProfile u =
    { username = u.username
    , bio = u.bio
    , following = False
    , points = u.points
    }


type alias Email =
    String
