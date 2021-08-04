module Pages.Profile.Username_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Config.View
import Data.Article exposing (Article)
import Data.Article.Filters as Filters
import Data.Profile exposing (Profile)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Element exposing (Element)
import Element.Border as Border
import Gen.Params.Profile.Username_ exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events as Events
import Page
import Request
import Shared
import View exposing (View)
import View.Color as Color
import View.NotFound
import Widget
import Widget.Material as Material
import Widget.Material.Typography as Typography


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared req
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { username : String
    , profile : Response Profile
    , listing : Response Data.Article.Listing
    , selectedTab : Tab
    , page : Int
    }


type Tab
    = MyArticles
    | FavoritedArticles


init : Shared.Model -> Request.With Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { username = params.username
      , profile = Data.Response.Loading
      , listing = Data.Response.Loading
      , selectedTab = MyArticles
      , page = 1
      }
    , Cmd.batch
        [ ProfileGet_Profile__Username_
            { username = params.username
            }
            |> sendToBackend
        , fetchArticlesBy params.username 1
        ]
    )


fetchArticlesBy : String -> Int -> Cmd Msg
fetchArticlesBy username page_ =
    ArticleList_Username_
        { page = page_
        , filters = Filters.create |> Filters.withAuthor username
        }
        |> sendToBackend


fetchArticlesFavoritedBy : String -> Int -> Cmd Msg
fetchArticlesFavoritedBy username page_ =
    ArticleList_Username_
        { page = page_
        , filters =
            Filters.create |> Filters.favoritedBy username
        }
        |> sendToBackend



-- UPDATE


type Msg
    = GotProfile (Response Profile)
    | GotArticles (Response Data.Article.Listing)
    | Clicked Tab
    | ClickedFavorite User Article
    | ClickedUnfavorite User Article
    | UpdatedArticle (Response Article)
    | ClickedFollow User Profile
    | ClickedUnfollow User Profile
    | ClickedPage Int


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case msg of
        GotProfile profile ->
            ( { model | profile = profile }
            , Cmd.none
            )

        ClickedFollow user profile ->
            ( model
            , ProfileFollow_Profile__Username_
                { username = profile.username
                }
                |> sendToBackend
            )

        ClickedUnfollow user profile ->
            ( model
            , ProfileUnfollow_Profile__Username_
                { username = profile.username
                }
                |> sendToBackend
            )

        GotArticles listing ->
            ( { model | listing = listing }
            , Cmd.none
            )

        Clicked MyArticles ->
            ( { model
                | selectedTab = MyArticles
                , listing = Data.Response.Loading
                , page = 1
              }
            , fetchArticlesBy model.username 1
            )

        Clicked FavoritedArticles ->
            ( { model
                | selectedTab = FavoritedArticles
                , listing = Data.Response.Loading
                , page = 1
              }
            , fetchArticlesFavoritedBy model.username 1
            )

        ClickedFavorite user article ->
            ( model
            , ArticleFavorite_Profile__Username_
                { slug = article.slug
                }
                |> sendToBackend
            )

        ClickedUnfavorite user article ->
            ( model
            , ArticleUnfavorite_Profile__Username_
                { slug = article.slug
                }
                |> sendToBackend
            )

        ClickedPage page_ ->
            let
                fetch : String -> Int -> Cmd Msg
                fetch =
                    case model.selectedTab of
                        MyArticles ->
                            fetchArticlesBy

                        FavoritedArticles ->
                            fetchArticlesFavoritedBy
            in
            ( { model
                | listing = Data.Response.Loading
                , page = page_
              }
            , fetch
                model.username
                page_
            )

        UpdatedArticle (Data.Response.Success article) ->
            ( { model
                | listing =
                    Data.Response.map (Data.Article.updateArticle article)
                        model.listing
              }
            , Cmd.none
            )

        UpdatedArticle _ ->
            ( model, Cmd.none )


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
                View.NotFound.view

            _ ->
                Element.none
    }


viewProfile : Shared.Model -> Profile -> Model -> Element Msg
viewProfile shared profile model =
    let
        isViewingOwnProfile : Bool
        isViewingOwnProfile =
            Maybe.map .username shared.user == Just profile.username

        viewTabRow : Html Msg
        viewTabRow =
            div [ class "articles-toggle" ]
                [ ul [ class "nav nav-pills outline-active" ]
                    (List.map viewTab [ MyArticles, FavoritedArticles ])
                ]

        viewTab : Tab -> Html Msg
        viewTab tab =
            li [ class "nav-item" ]
                [ button
                    [ class "nav-link"
                    , Events.onClick (Clicked tab)
                    , classList [ ( "active", tab == model.selectedTab ) ]
                    ]
                    [ text
                        (case tab of
                            MyArticles ->
                                "My Articles"

                            FavoritedArticles ->
                                "Favorited Articles"
                        )
                    ]
                ]
    in
    [ Element.text profile.username |> Element.el Typography.h2
    , profile.bio |> Maybe.map Element.text |> Maybe.withDefault Element.none
    , Element.text <| "Points: " ++ String.fromInt profile.points
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Border.rounded Config.View.rounded
                   , Element.padding Config.View.padding
                   , Element.width <| Element.maximum Config.View.maxWidth <| Element.fill
                   , Element.centerX
                   , Element.centerY
                   ]
            )
