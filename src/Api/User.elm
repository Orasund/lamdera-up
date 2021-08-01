module Api.User exposing (..)

{-|

@docs User, UserFull, Email

-}

import Api.Article exposing (Slug)
import Api.Profile exposing (Profile)


type alias User =
    { id : Int
    , email : Email
    , username : String
    , bio : Maybe String
    , points : Int
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
