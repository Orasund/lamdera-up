module Pages.NotFound exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Route exposing (Route)
import Html exposing (..)
import Page
import Request exposing (Request)
import Shared
import View exposing (View)
import View.NotFound


type alias Model =
    ()


type alias Msg =
    Route


page : Shared.Model -> Request -> Page.With () Route
page shared req =
    Page.advanced
        { init = init shared
        , update = update req
        , subscriptions = subscriptions
        , view = view
        }


init : Shared.Model -> ( (), Effect Route )
init _ =
    ( (), Effect.none )


update : Request -> Route -> () -> ( (), Effect Route )
update _ route model =
    ( model, Shared.RequestedRouteChange route |> Effect.fromShared )


subscriptions : () -> Sub Route
subscriptions _ =
    Sub.none


view : () -> View Route
view _ =
    { title = "404"
    , body =
        View.NotFound.view identity
    }
