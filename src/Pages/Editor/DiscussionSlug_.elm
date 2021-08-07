module Pages.Editor.DiscussionSlug_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Data.Discussion exposing (Discussion)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Element
import Gen.Params.Editor.DiscussionSlug_ exposing (Params)
import Gen.Route as Route
import Html exposing (..)
import Page
import Request
import Shared
import Utils.Route
import View exposing (View)
import View.Editor exposing (Field, Form)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.element <|
        \user ->
            { init = init shared req
            , update = update req
            , subscriptions = subscriptions
            , view = view user
            }



-- INIT


type alias Model =
    { slug : String
    , form : Maybe Form
    , discussion : Response Discussion
    }


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init _ { params } =
    ( { slug = params.discussionSlug
      , form = Nothing
      , discussion = Data.Response.Loading
      }
    , DiscussionGet_Editor__DiscussionSlug_
        { slug = params.discussionSlug
        }
        |> sendToBackend
    )



-- UPDATE


type Msg
    = SubmittedForm User Form
    | Updated Field String
    | UpdatedDiscussion (Response Discussion)
    | LoadedInitialDiscussion (Response Discussion)


update : Request.With Params -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        LoadedInitialDiscussion discussion ->
            case discussion of
                Data.Response.Success a ->
                    ( { model
                        | form =
                            Just <|
                                { title = a.title
                                , description = a.description
                                , body = a.body
                                , tags = String.join ", " a.tags
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Updated field value ->
            ( { model
                | form =
                    Maybe.map
                        (View.Editor.updateField field value)
                        model.form
              }
            , Cmd.none
            )

        SubmittedForm _ form ->
            ( model
            , DiscussionUpdate_Editor__DiscussionSlug_
                { slug = model.slug
                , updates =
                    { title = form.title
                    , description = form.description
                    , body = form.body
                    , tags =
                        form.tags
                            |> String.split ","
                            |> List.map String.trim
                    }
                }
                |> sendToBackend
            )

        UpdatedDiscussion discussion ->
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
    { title = "Editing Discussion"
    , body =
        case model.form of
            Just form ->
                View.Editor.view
                    { onFormSubmit = SubmittedForm user form
                    , title = "Edit Discussion"
                    , form = form
                    , onUpdate = Updated
                    , buttonLabel = "Save"
                    , discussion = model.discussion
                    }

            Nothing ->
                Element.none
    }
