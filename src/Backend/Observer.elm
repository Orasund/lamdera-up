module Backend.Observer exposing (updatedGame)

import Data.Game exposing (Game)
import Data.Response exposing (Response(..))
import Data.User as User exposing (UserFull)
import Gen.Msg
import Lamdera exposing (ClientId, sendToFrontend)
import Pages.Profile.Id_
import Shared exposing (Msg(..))
import Types exposing (ToFrontend(..))


updatedGame : { userFull : UserFull, clientId : ClientId } -> Game -> Cmd msg
updatedGame args game =
    let
        user =
            User.toUser game args.userFull

        profile =
            User.toProfile game args.userFull
    in
    [ user
        |> ActiveSession
        |> sendToFrontend args.clientId
    , profile
        |> Success
        |> Pages.Profile.Id_.GotProfile
        |> Gen.Msg.Profile__Id_
        |> PageMsg
        |> sendToFrontend args.clientId
    ]
        |> Cmd.batch
