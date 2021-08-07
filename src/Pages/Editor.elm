module Pages.Editor exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Data.Discussion exposing (Discussion)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Gen.Route as Route
import Page
import Request exposing (Request)
import Shared
import Utils.Route
import View exposing (View)
import View.Editor exposing (Field, Form)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.protected.element <|
        \user ->
            { init = init shared
            , update = update req
            , subscriptions = subscriptions
            , view = view user
            }



-- INIT


type alias Model =
    { form : Form
    , discussion : Response Discussion
    }


init : Shared.Model -> ( Model, Cmd Msg )
init _ =
    ( { form =
            { title = ""
            , description = ""
            , body = ""
            , tags = ""
            }
      , discussion = Data.Response.NotAsked
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SubmittedForm User
    | Updated Field String
    | GotDiscussion (Response Discussion)


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        Updated field value ->
            ( { model
                | form =
                    View.Editor.updateField
                        field
                        value
                        model.form
              }
            , Cmd.none
            )

        SubmittedForm _ ->
            ( model
            , DiscussionCreate_Editor
                { discussion =
                    { title = model.form.title
                    , description = model.form.description
                    , body = model.form.body
                    , tags =
                        model.form.tags
                            |> String.split ","
                            |> List.map String.trim
                    }
                }
                |> sendToBackend
            )

        GotDiscussion discussion ->
            ( { model | discussion = discussion }
            , case discussion of
                Data.Response.Success newDiscussion ->
                    Utils.Route.navigate req.key
                        (Route.Discussion__Slug_ { slug = newDiscussion.slug })

                _ ->
                    Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : User -> Model -> View Msg
view user model =
    { title = "New Discussion"
    , body =
        View.Editor.view
            { onFormSubmit = SubmittedForm user
            , title = "New Discussion"
            , form = model.form
            , onUpdate = Updated
            , buttonLabel = "Publish"
            , discussion = model.discussion
            }
    }
