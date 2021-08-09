module Pages.Profile.Id_ exposing (Model, Msg(..), page)

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
    , amountToSpend : String
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
      , amountToSpend = "1"
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
    | TriggeredRule { rule : Pointer Rule, amountSpent : Int }
    | AmountChanged String
    | RequestedRouteChange Route


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

        tokens =
            shared.user
                |> Maybe.map .tokens
                |> Maybe.withDefault 0
    in
    [ [ Element.text profile.username |> Element.el Typography.h2
      , profile.bio |> Maybe.map Element.text |> Maybe.withDefault Element.none
      , Element.text <| "Points: " ++ String.fromInt profile.points
      , if isViewingOwnProfile then
            Element.text <| "Tokens: " ++ String.fromInt tokens

        else
            Element.none
      ]
        |> Element.column
            [ Element.spacing Config.View.spacing
            ]
    , if isViewingOwnProfile then
        (([ Element.text "Actions" |> Element.el Typography.h4
          , [ Element.text "Spend "
            , Input.text
                (Material.textInputAttributes Color.palette
                    ++ [ Element.width <| Element.px 64 ]
                )
                { text = model.amountToSpend
                , placeholder = Nothing
                , label = "Amount to spend" |> Input.labelHidden
                , onChange = AmountChanged
                }
            , Element.text " Token(s)"
            ]
                |> Element.row [ Element.width Element.shrink ]
          ]
            |> Element.row [ Element.spaceEvenly, Element.width Element.fill ]
         )
            :: (rules
                    |> List.map
                        (Rule.view
                            { triggerRule = TriggeredRule
                            , amountSpent =
                                model.amountToSpend
                                    |> String.toInt
                                    |> Maybe.withDefault 0
                            , tokens = tokens
                            }
                        )
               )
        )
            |> Element.column
                [ Element.spacing Config.View.spacing
                , Element.width Element.fill
                ]

      else
        Element.none
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
