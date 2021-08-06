module Data.User exposing (..)

{-|

@docs User, UserFull, Email

-}

import Data.Article exposing (Slug)
import Data.Game exposing (Game)
import Data.Game.Player exposing (Player)
import Data.Game.Pointer as Pointer exposing (Pointer)
import Data.Profile exposing (Profile)
import Dict


type alias User =
    { id : Int
    , email : Email
    , username : String
    , bio : Maybe String
    , tokens : Int
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
    , player : Pointer Player
    }


type alias UserId =
    Int


new : { id : Int, email : Email, username : String, password : String, player : Pointer Player } -> UserFull
new args =
    { id = args.id
    , email = args.email
    , username = args.username
    , bio = Nothing
    , password = args.password
    , favorites = []
    , following = []
    , player = args.player
    }


toUser : Game -> UserFull -> User
toUser game u =
    let
        player =
            game.players
                |> Pointer.get u.player
    in
    { id = u.id
    , email = u.email
    , username = u.username
    , bio = u.bio
    , tokens = player |> Maybe.map .tokens |> Maybe.withDefault 0
    , points = player |> Maybe.map .points |> Maybe.withDefault 0
    }


toProfile : Game -> UserFull -> Profile
toProfile game u =
    let
        player =
            game.players
                |> Pointer.get u.player
    in
    { id = u.id
    , username = u.username
    , bio = u.bio
    , following = False
    , points =
        player
            |> Maybe.map .points
            |> Maybe.withDefault 0
    }


type alias Email =
    String
