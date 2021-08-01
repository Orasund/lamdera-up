module Pages.Settings exposing (Model, Msg(..), page)

import Api.Data exposing (Data)
import Api.User exposing (User)
import Bridge exposing (..)
import Config.View
import Effect exposing (Effect)
import Element
import Element.Border as Border
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events as Events
import Page
import Request exposing (Request)
import Shared
import Utils.Maybe
import View exposing (View)
import View.Color as Color
import View.ErrorList
import View.Input
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.protected.advanced
        (\user ->
            { init = init shared
            , update = update
            , subscriptions = subscriptions
            , view = view user
            }
        )



-- INIT


type alias Model =
    { username : String
    , bio : String
    , email : String
    , oldPassword : Maybe String
    , newPassword : Maybe String
    , message : Maybe String
    , errors : List String
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( case shared.user of
        Just user ->
            { username = user.username
            , bio = user.bio |> Maybe.withDefault ""
            , email = user.email
            , oldPassword = Nothing
            , newPassword = Nothing
            , message = Nothing
            , errors = []
            }

        Nothing ->
            { username = ""
            , bio = ""
            , email = ""
            , newPassword = Nothing
            , oldPassword = Nothing
            , message = Nothing
            , errors = []
            }
    , Effect.none
    )



-- UPDATE


type Msg
    = Updated Field String
    | SubmittedForm User
    | GotUser (Data User)


type Field
    = Username
    | Bio
    | Email
    | NewPassword
    | OldPassword


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Updated Username value ->
            ( { model | username = value }, Effect.none )

        Updated Bio value ->
            ( { model | bio = value }, Effect.none )

        Updated Email value ->
            ( { model | email = value }, Effect.none )

        Updated OldPassword value ->
            ( { model | oldPassword = Just value }, Effect.none )

        Updated NewPassword value ->
            ( { model | newPassword = Just value }, Effect.none )

        SubmittedForm user ->
            ( { model | message = Nothing, errors = [] }
            , (Effect.fromCmd << sendToBackend) <|
                UserUpdate_Settings
                    { params =
                        { username = model.username
                        , email = model.email
                        , oldPassword = model.oldPassword
                        , newPassword = model.newPassword
                        , bio = model.bio
                        }
                    }
            )

        GotUser (Api.Data.Success user) ->
            ( { model | message = Just "User updated!" }
            , Effect.fromShared (Shared.SignedInUser user)
            )

        GotUser (Api.Data.Failure reasons) ->
            ( { model | errors = reasons }
            , Effect.none
            )

        GotUser _ ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : User -> Model -> View Msg
view user model =
    { title = "Settings"
    , body =
        [ Element.text "Your Settings"
            |> Element.el (Typography.h2 ++ [ Element.centerX ])
        , [ View.Input.textInput
                { text = model.username
                , label = "Your Username"
                , onChange = Updated Username
                }
                |> View.Input.withLabel "Your Username"
          , View.Input.multiLineInput
                { text = model.bio
                , label = "Short bio about you"
                , onChange = Updated Bio
                }
                |> View.Input.withLabel "Short bio about you"
          , View.Input.textInput
                { text = model.email
                , label = "Email"
                , onChange = Updated Email
                }
                |> View.Input.withLabel "Email"
          , [ Widget.currentPasswordInputV2
                (Material.passwordInput Color.palette
                    |> Customize.elementRow [ Element.width Element.fill ]
                )
                { text = Maybe.withDefault "" model.oldPassword
                , placeholder =
                    "Old Password"
                        |> Element.text
                        |> Input.placeholder []
                        |> Just
                , label = "Old Password"
                , onChange = Updated OldPassword
                , show = False
                }
            , Widget.newPasswordInputV2
                (Material.passwordInput Color.palette
                    |> Customize.elementRow [ Element.width Element.fill ]
                )
                { text = Maybe.withDefault "" model.newPassword
                , placeholder =
                    "New Password"
                        |> Element.text
                        |> Input.placeholder []
                        |> Just
                , label = "New Password"
                , onChange = Updated NewPassword
                , show = False
                }
            ]
                |> Element.column
                    [ Element.spacing Config.View.spacing
                    , Element.width Element.fill
                    ]
                |> View.Input.withLabel "Change Password"
          , View.ErrorList.view model.errors |> Element.html
          , (\message ->
                p [ class "text-success" ] [ text message ]
            )
                |> Utils.Maybe.view model.message
                |> Element.html
          , Widget.textButton (Material.containedButton Color.palette)
                { text = "Update Settings"
                , onPress = Just <| SubmittedForm user
                }
                |> Element.el [ Element.alignRight ]
          ]
            |> Element.column
                [ Element.spacing Config.View.spacing
                , Element.width Element.fill
                ]
        ]
            |> Element.column
                (Material.cardAttributes Color.palette
                    ++ [ Element.spacing Config.View.spacing
                       , Border.rounded Config.View.rounded
                       , Element.padding Config.View.padding
                       , Element.width Element.shrink
                       , Element.centerX
                       , Element.centerY
                       , Element.width <| Element.maximum 1024 <| Element.fill
                       ]
                )
    }
