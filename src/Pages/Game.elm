module Pages.Game exposing (..)

import Bridge exposing (..)
import Config.View
import Data.Game exposing (Rule)
import Data.Game.Pointer exposing (Pointer)
import Data.Profile exposing (Profile)
import Data.Response exposing (Response)
import Effect exposing (Effect)
import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Gen.Params.Profile.Id_ exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Page
import Request
import Shared
import View exposing (View)
import View.Color as Color
import View.NotFound
import View.Rule as Rule
import Widget.Material as Material
import Widget.Material.Typography as Typography


type alias Model =
    { profiles : Response (List Profile)
    , rules : Response (List Rule)
    }


type Msg
    = GotProfiles (Response (List Profile))
    | GotRules (Response (List Rule))
    | RequestedRouteChange Route


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init _ { params } =
    let
        id =
            params.id |> String.toInt |> Maybe.withDefault -1
    in
    ( { profiles = Data.Response.Loading
      , rules = Data.Response.Loading
      }
    , Cmd.batch
        [ ProfileGet_Profile__Id_ { id = id } |> sendToBackend
        , DiscussionList_Username_ |> sendToBackend
        ]
        |> Effect.fromCmd
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        GotProfile profile ->
            ( { model | profile = profile }
            , Effect.none
            )

        GotRules rules ->
            ( { model | rules = rules }
            , Effect.none
            )

        TriggeredRule args ->
            ( model
            , SpendToken args
                |> sendToBackend
                |> Effect.fromCmd
            )

        AmountChanged string ->
            ( { model | amountToSpend = string }, Effect.none )

        RequestedRouteChange route ->
            ( model, Shared.RequestedRouteChange route |> Effect.fromShared )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Profile"
    , body =
        case model.profile of
            Data.Response.Success profile ->
                viewProfile shared profile model

            Data.Response.Failure _ ->
                View.NotFound.view RequestedRouteChange

            _ ->
                Element.none
    }
