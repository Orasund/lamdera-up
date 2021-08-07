module Pages.Profile.Id_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Config.View
import Data.Game exposing (Rule)
import Data.Game.Pointer exposing (Pointer)
import Data.Profile exposing (Profile)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Effect exposing (Effect)
import Element exposing (Element)
import Element.Border as Border
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


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { id : Int
    , profile : Response Profile
    , rules : Response (List Rule)
    }


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init _ { params } =
    let
        id =
            params.id |> String.toInt |> Maybe.withDefault -1
    in
    ( { id = id
      , profile = Data.Response.Loading
      , rules = Data.Response.Loading
      }
    , Cmd.batch
        [ ProfileGet_Profile__Id_ { id = id } |> sendToBackend
        , DiscussionList_Username_ |> sendToBackend
        ]
        |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = GotProfile (Response Profile)
    | GotRules (Response (List Rule))
    | ClickedFollow User Profile
    | ClickedUnfollow User Profile
    | TriggeredRule (Pointer Rule)
    | RequestedRouteChange Route


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        GotProfile profile ->
            ( { model | profile = profile }
            , Effect.none
            )

        ClickedFollow _ profile ->
            ( model
            , ProfileFollow_Profile__Id_
                { id = profile.id
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedUnfollow _ profile ->
            ( model
            , ProfileUnfollow_Profile__Id_
                { id = profile.id
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        GotRules rules ->
            ( { model | rules = rules }
            , Effect.none
            )

        TriggeredRule rule ->
            shared.user
                |> Maybe.map
                    (\{ player } ->
                        ( model
                        , SpendToken { rule = rule, player = player }
                            |> sendToBackend
                            |> Effect.fromCmd
                        )
                    )
                |> Maybe.withDefault ( model, Effect.none )

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


viewProfile : Shared.Model -> Profile -> Model -> Element Msg
viewProfile shared profile model =
    let
        isViewingOwnProfile : Bool
        isViewingOwnProfile =
            Maybe.map .username shared.user == Just profile.username

        rules =
            case model.rules of
                Data.Response.Success list ->
                    list

                _ ->
                    []
    in
    [ [ Element.text profile.username |> Element.el Typography.h2
      , profile.bio |> Maybe.map Element.text |> Maybe.withDefault Element.none
      , Element.text <| "Points: " ++ String.fromInt profile.points
      , if isViewingOwnProfile then
            Element.text <|
                "Tokens: "
                    ++ (shared.user
                            |> Maybe.map .tokens
                            |> Maybe.withDefault 0
                            |> String.fromInt
                       )

        else
            Element.none
      ]
        |> Element.column
            [ Element.spacing Config.View.spacing
            ]
    , if isViewingOwnProfile then
        rules
            |> List.map (Rule.view TriggeredRule)
            |> Element.column
                [ Element.spacing Config.View.spacing
                ]

      else
        Element.none
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Border.rounded Config.View.rounded
                   , Element.padding Config.View.padding
                   , Element.spacing Config.View.spacing
                   , Element.width <| Element.maximum Config.View.maxWidth <| Element.fill
                   , Element.centerX
                   , Element.centerY
                   ]
            )
