module Pages.Game exposing (Model, Msg(..), page)

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
import Gen.Params.Game exposing (Params)
import Gen.Route exposing (Route)
import Html exposing (..)
import Page
import Request
import Shared
import View exposing (View)
import View.Color as Color
import View.NotFound
import View.Profile
import View.Rule as Rule
import Widget.Material as Material
import Widget.Material.Typography as Typography


type alias Model =
    { profiles : List Profile
    , rules : Response (List Rule)
    }


type Msg
    = GotProfiles (List Profile)
    | GotRules (Response (List Rule))
    | RequestedRouteChange Route
    | TriggeredRule { rule : Pointer Rule, amountSpent : Int }


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init _ {} =
    ( { profiles = []
      , rules = Data.Response.Loading
      }
    , Cmd.batch
        [ GameGet_Profiles |> sendToBackend
        , DiscussionList_Username_ |> sendToBackend
        ]
        |> Effect.fromCmd
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        GotProfiles profiles ->
            ( { model | profiles = profiles }
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

        RequestedRouteChange route ->
            ( model, Shared.RequestedRouteChange route |> Effect.fromShared )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Game"
    , body =
        [ case model.rules of
            Data.Response.Success rules ->
                [ "Rules" |> Element.text |> Element.el Typography.h2
                , rules
                    |> List.map Rule.staticView
                    |> Element.column [ Element.width Element.fill, Element.spacing Config.View.spacing ]
                ]
                    |> Element.column
                        (Material.cardAttributes Color.palette
                            ++ [ Border.rounded Config.View.rounded
                               , Element.padding Config.View.padding
                               , Element.spacing (2 * Config.View.spacing)
                               , Element.width <| Element.maximum Config.View.maxWidth <| Element.fill
                               , Element.centerX
                               , Element.centerY
                               ]
                        )

            _ ->
                Element.none
        , [ "Users" |> Element.text |> Element.el Typography.h2
          , model.profiles
                |> View.Profile.list
                    { currentUser = shared.user
                    , rules =
                        case model.rules of
                            Data.Response.Success list ->
                                list

                            _ ->
                                []
                    }
          ]
            |> Element.column
                (Material.cardAttributes Color.palette
                    ++ [ Border.rounded Config.View.rounded
                       , Element.padding Config.View.padding
                       , Element.spacing (2 * Config.View.spacing)
                       , Element.width <| Element.maximum Config.View.maxWidth <| Element.fill
                       , Element.centerX
                       , Element.centerY
                       ]
                )
        ]
            |> Element.column
                [ Element.width <| Element.maximum Config.View.maxWidth <| Element.fill
                , Element.centerX
                , Element.centerY
                , Element.spacing Config.View.spacing
                ]
    }
