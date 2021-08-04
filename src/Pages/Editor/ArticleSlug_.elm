module Pages.Editor.ArticleSlug_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Data.Article exposing (Article)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Element
import Gen.Params.Editor.ArticleSlug_ exposing (Params)
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
    , article : Response Article
    }


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { slug = params.articleSlug
      , form = Nothing
      , article = Data.Response.Loading
      }
    , ArticleGet_Editor__ArticleSlug_
        { slug = params.articleSlug
        }
        |> sendToBackend
    )



-- UPDATE


type Msg
    = SubmittedForm User Form
    | Updated Field String
    | UpdatedArticle (Response Article)
    | LoadedInitialArticle (Response Article)


update : Request.With Params -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        LoadedInitialArticle article ->
            case article of
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

        SubmittedForm user form ->
            ( model
            , ArticleUpdate_Editor__ArticleSlug_
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

        UpdatedArticle article ->
            ( { model | article = article }
            , case article of
                Data.Response.Success newArticle ->
                    Utils.Route.navigate req.key
                        (Route.Article__Slug_ { slug = newArticle.slug })

                _ ->
                    Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : User -> Model -> View Msg
view user model =
    { title = "Editing Article"
    , body =
        case model.form of
            Just form ->
                View.Editor.view
                    { onFormSubmit = SubmittedForm user form
                    , title = "Edit Article"
                    , form = form
                    , onUpdate = Updated
                    , buttonLabel = "Save"
                    , article = model.article
                    }

            Nothing ->
                Element.none
    }
