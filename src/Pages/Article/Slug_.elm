module Pages.Article.Slug_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Config.View
import Data.Article exposing (Article)
import Data.Article.Comment exposing (Comment)
import Data.Profile exposing (Profile)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Effect exposing (Effect)
import Element exposing (Element)
import Gen.Params.Article.Slug_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Markdown
import Page
import Request
import Shared
import Utils.Route
import Utils.Time
import View exposing (View)
import View.Color as Color
import View.IconButton as IconButton
import View.Input
import Widget
import Widget.Material as Material


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req
        , update = update req
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { article : Response Article
    , comments : Response (List Comment)
    , commentText : String
    }


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init shared { params } =
    ( { article = Data.Response.Loading
      , comments = Data.Response.Loading
      , commentText = ""
      }
    , Cmd.batch
        [ ArticleGet_Article__Slug_
            { slug = params.slug
            }
            |> sendToBackend
        , ArticleCommentGet_Article__Slug_
            { articleSlug = params.slug
            }
            |> sendToBackend
        ]
        |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = GotArticle (Response Article)
    | ClickedFavorite User Article
    | ClickedUnfavorite User Article
    | ClickedDeleteArticle User Article
    | DeletedArticle (Response Article)
    | GotAuthor (Response Profile)
    | ClickedFollow User Profile
    | ClickedUnfollow User Profile
    | GotComments (Response (List Comment))
    | ClickedDeleteComment User Article Comment
    | DeletedComment (Response Int)
    | SubmittedCommentForm User Article
    | CreatedComment (Response Comment)
    | UpdatedCommentText String
    | RequestedRouteChange Route


update : Request.With Params -> Msg -> Model -> ( Model, Effect Msg )
update req msg model =
    case msg of
        GotArticle article ->
            ( { model | article = article }
            , Effect.none
            )

        ClickedFavorite user article ->
            ( model
            , ArticleFavorite_Article__Slug_
                { slug = article.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedUnfavorite user article ->
            ( model
            , ArticleUnfavorite_Article__Slug_
                { slug = article.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedDeleteArticle user article ->
            ( model
            , ArticleDelete_Article__Slug_
                { slug = article.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        DeletedArticle _ ->
            ( model
            , Utils.Route.navigate req.key Route.Home_
                |> Effect.fromCmd
            )

        GotAuthor profile ->
            let
                updateAuthor : Article -> Article
                updateAuthor article =
                    case profile of
                        Data.Response.Success author ->
                            { article | author = author }

                        _ ->
                            article
            in
            ( { model | article = Data.Response.map updateAuthor model.article }
            , Effect.none
            )

        ClickedFollow user profile ->
            ( model
            , ProfileFollow_Article__Slug_
                { username = profile.username
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedUnfollow user profile ->
            ( model
            , ProfileUnfollow_Article__Slug_
                { username = profile.username
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        GotComments comments ->
            ( { model | comments = comments }
            , Effect.none
            )

        UpdatedCommentText text ->
            ( { model | commentText = text }
            , Effect.none
            )

        SubmittedCommentForm user article ->
            if String.isEmpty model.commentText then
                ( model, Effect.none )

            else
                ( { model | commentText = "" }
                , ArticleCommentCreate_Article__Slug_
                    { articleSlug = article.slug
                    , comment = { body = model.commentText }
                    }
                    |> sendToBackend
                    |> Effect.fromCmd
                )

        CreatedComment comment ->
            ( case comment of
                Data.Response.Success c ->
                    { model | comments = Data.Response.map (\comments -> c :: comments) model.comments }

                _ ->
                    model
            , Effect.none
            )

        ClickedDeleteComment user article comment ->
            ( model
            , ArticleCommentDelete_Article__Slug_
                { articleSlug = article.slug
                , commentId = comment.id
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        DeletedComment id ->
            let
                removeComment : List Comment -> List Comment
                removeComment =
                    List.filter (\comment -> Data.Response.Success comment.id /= id)
            in
            ( { model | comments = Data.Response.map removeComment model.comments }
            , Effect.none
            )

        RequestedRouteChange route ->
            ( model, Shared.RequestedRouteChange route |> Effect.fromShared )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model.article of
        Data.Response.Success article ->
            { title = article.title
            , body = viewArticle shared model article
            }

        _ ->
            { title = "Article"
            , body = Element.none
            }


viewArticle : Shared.Model -> Model -> Article -> Element Msg
viewArticle shared model article =
    [ div [ class "container" ]
        [ h1 [] [ text article.title ]
        , viewArticleMeta shared model article
        ]
        |> Element.html
        |> Element.el
            [ Element.width <| Element.fill
            , Element.htmlAttribute <| class "banner"
            ]
    , [ div [ class "row article-content" ]
            [ div [ class "col-md-12" ]
                [ Markdown.toHtml [] article.body ]
            , if List.isEmpty article.tags then
                text ""

              else
                ul [ class "tag-list" ]
                    (List.map
                        (\tag -> li [ class "tag-default tag-pill tag-outline" ] [ text tag ])
                        article.tags
                    )
            ]
            |> Element.html
      , hr [] [] |> Element.html
      , div [ class "article-actions" ] [ viewArticleMeta shared model article ]
            |> Element.html
      , viewCommentSection shared model article
      ]
        |> Element.column
            [ class "container page" |> Element.htmlAttribute
            , Element.width <| Element.fill
            ]
    ]
        |> Element.column
            [ Element.width <| Element.fill
            , class "article-page" |> Element.htmlAttribute
            ]


viewArticleMeta : Shared.Model -> Model -> Article -> Html Msg
viewArticleMeta shared model article =
    div [ class "article-meta" ] <|
        List.concat
            [ [ div [ class "info" ]
                    [ a [ class "author", href ("/profile/" ++ article.author.username) ] [ text article.author.username ]
                    , span [ class "date" ] [ text (Utils.Time.formatDate article.createdAt) ]
                    ]
              ]
            , case shared.user of
                Just user ->
                    viewControls article user

                Nothing ->
                    []
            ]


viewControls : Article -> User -> List (Html Msg)
viewControls article user =
    if article.author.username == user.username then
        [ a
            [ class "btn btn-outline-secondary btn-sm"
            , href ("/editor/" ++ article.slug)
            ]
            [ i [ class "ion-edit" ] []
            , text "Edit article"
            ]
        , IconButton.view
            { color = IconButton.OutlinedRed
            , icon = IconButton.Trash
            , label = "Delete article"
            , onClick = ClickedDeleteArticle user article
            }
        ]

    else
        [ if article.author.following then
            IconButton.view
                { color = IconButton.FilledGray
                , icon = IconButton.Plus
                , label = "Unfollow " ++ article.author.username
                , onClick = ClickedUnfollow user article.author
                }

          else
            IconButton.view
                { color = IconButton.OutlinedGray
                , icon = IconButton.Plus
                , label = "Follow " ++ article.author.username
                , onClick = ClickedFollow user article.author
                }
        , if article.favorited then
            IconButton.view
                { color = IconButton.FilledGreen
                , icon = IconButton.Heart
                , label = "Unfavorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
                , onClick = ClickedUnfavorite user article
                }

          else
            IconButton.view
                { color = IconButton.OutlinedGreen
                , icon = IconButton.Heart
                , label = "Favorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
                , onClick = ClickedFavorite user article
                }
        ]


viewCommentSection : Shared.Model -> Model -> Article -> Element Msg
viewCommentSection shared model article =
    [ case model.comments of
        Data.Response.Success comments ->
            List.map (viewComment shared.user article) comments
                |> Element.column
                    [ Element.fill
                        |> Element.maximum Config.View.maxWidth
                        |> Element.width
                    ]

        _ ->
            Element.none
    , case shared.user of
        Just user ->
            viewCommentForm model user article

        Nothing ->
            Element.none
    ]
        |> Element.column [ Element.width Element.fill ]


viewCommentForm : Model -> User -> Article -> Element Msg
viewCommentForm model user article =
    [ View.Input.multiLineInput
        { onChange = UpdatedCommentText
        , text = model.commentText
        , label = "Write a comment..."
        }
    , Widget.textButton (Material.containedButton Color.palette)
        { text = "Post Comment"
        , onPress = Just <| SubmittedCommentForm user article
        }
        |> Element.el [ Element.alignRight, Element.width Element.fill ]
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Element.fill
                        |> Element.maximum Config.View.maxWidth
                        |> Element.width
                   , Element.centerX
                   ]
            )


viewComment : Maybe User -> Article -> Comment -> Element Msg
viewComment currentUser article comment =
    let
        viewCommentActions : Element Msg
        viewCommentActions =
            case currentUser of
                Just user ->
                    if user.username == comment.author.username then
                        Widget.textButton (Material.textButton Color.palette)
                            { text = "Delete"
                            , onPress = Just <| ClickedDeleteComment user article comment
                            }

                    else
                        Element.none

                Nothing ->
                    Element.none
    in
    [ Element.text comment.body
    , [ Widget.textButton (Material.textButton Color.palette)
            { text = comment.author.username
            , onPress =
                Route.Profile__Username_ { username = comment.author.username }
                    |> RequestedRouteChange
                    |> Just
            }
      , span [ class "date-posted" ] [ text (Utils.Time.formatDate comment.createdAt) ]
            |> Element.html
      , viewCommentActions
      ]
        |> Element.row []
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Element.fill
                        |> Element.maximum Config.View.maxWidth
                        |> Element.width
                   , Element.centerX
                   ]
            )
