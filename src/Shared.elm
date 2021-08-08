module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Bridge exposing (ToBackend(..))
import Data.User exposing (User)
import Element
import Gen.Route exposing (Route)
import Request exposing (Request)
import Utils.Route
import View exposing (View)
import View.Navbar



-- INIT


type alias Flags =
    ()


type alias Model =
    { user : Maybe User
    }


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( Model Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedSignOut
    | SignedInUser User
    | RequestedRouteChange Route


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        SignedInUser user ->
            ( { model | user = Just user }
            , Cmd.none
            )

        ClickedSignOut ->
            ( { model | user = Nothing }
            , model.user
                |> Maybe.map (\user -> Bridge.sendToBackend (SignedOut user))
                |> Maybe.withDefault Cmd.none
            )

        RequestedRouteChange route ->
            ( model, Request.pushRoute route req )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



-- VIEW


view :
    Request
    -> { page : View msg, toMsg : Msg -> msg }
    -> Model
    -> View msg
view req { page, toMsg } model =
    { title =
        if String.isEmpty page.title then
            "Conduit"

        else
            page.title ++ " | Conduit"
    , body =
        [ View.Navbar.view
            { user = model.user
            , currentRoute = Utils.Route.fromUrl req.url
            , onSignOut = toMsg ClickedSignOut
            , msgMapper = RequestedRouteChange >> toMsg
            }
        , page.body
        ]
            |> Element.column
                [ Element.width <| Element.fill
                , Element.height <| Element.fill
                ]
    }
