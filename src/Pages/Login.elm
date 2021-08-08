module Pages.Login exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Effect exposing (Effect)
import Frontend.KeyPressed as KeyPressed
import Gen.Route as Route exposing (Route)
import Page
import Request exposing (Request)
import Sha256
import Shared
import Utils.Route
import View exposing (View)
import View.UserForm


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared
        , update = update req
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { user : Response User
    , email : String
    , password : String
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( Model
        (case shared.user of
            Just user ->
                Data.Response.Success user

            Nothing ->
                Data.Response.NotAsked
        )
        ""
        ""
    , Effect.none
    )



-- UPDATE


type Msg
    = Updated Field String
    | AttemptedSignIn
    | GotUser (Response User)
    | RequestedRouteChange Route
    | NoOp


type Field
    = Email
    | Password


update : Request -> Msg -> Model -> ( Model, Effect Msg )
update req msg model =
    case msg of
        Updated Email email ->
            ( { model | email = email }
            , Effect.none
            )

        Updated Password password ->
            ( { model | password = password }
            , Effect.none
            )

        AttemptedSignIn ->
            ( model
            , (Effect.fromCmd << sendToBackend) <|
                UserAuthentication_Login
                    { params =
                        { email = model.email
                        , password = Sha256.sha256 model.password
                        }
                    }
            )

        GotUser user ->
            case Data.Response.toMaybe user of
                Just user_ ->
                    ( { model | user = user }
                    , Effect.batch
                        [ Effect.fromCmd (Utils.Route.navigate req.key Route.Home_)
                        , Effect.fromShared (Shared.SignedInUser user_)
                        ]
                    )

                Nothing ->
                    ( { model | user = user }
                    , Effect.none
                    )

        RequestedRouteChange route ->
            ( model, Shared.RequestedRouteChange route |> Effect.fromShared )

        NoOp ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    KeyPressed.submitOnKeyDown
        { noOp = NoOp
        , submitted = AttemptedSignIn
        }



-- VIEW


view : Model -> View Msg
view model =
    { title = "Sign in"
    , body =
        View.UserForm.view
            { user = model.user
            , label = "Sign in"
            , onFormSubmit = AttemptedSignIn
            , alternateLink = { label = "Need an account?", route = Route.Register }
            , fields =
                [ { label = "Email"
                  , type_ = "email"
                  , value = model.email
                  , onInput = Updated Email
                  }
                , { label = "Password"
                  , type_ = "password"
                  , value = model.password
                  , onInput = Updated Password
                  }
                ]
            , msgMapper = RequestedRouteChange
            }
    }
