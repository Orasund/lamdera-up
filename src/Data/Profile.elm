module Data.Profile exposing (Profile)

{-|

@docs Profile

-}


type alias Profile =
    { username : String
    , bio : Maybe String
    , following : Bool
    , points : Int
    }
