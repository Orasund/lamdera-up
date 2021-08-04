module Pages.Home_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Data.Article exposing (Article)
import Data.Article.Filters as Filters
import Data.Response exposing (Response)
import Data.User exposing (User)
import Element
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import Page
import Request exposing (Request)
import Shared
import Utils.Maybe
import View exposing (View)
import View.ArticleList


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { listing : Response Data.Article.Listing
    , page : Int
    , tags : Response (List Tag)
    , activeTab : Tab
    }


type Tab
    = FeedFor User
    | Global
    | TagFilter Tag


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    let
        activeTab : Tab
        activeTab =
            shared.user
                |> Maybe.map FeedFor
                |> Maybe.withDefault Global

        model : Model
        model =
            { listing = Data.Response.Loading
            , page = 1
            , tags = Data.Response.Loading
            , activeTab = activeTab
            }
    in
    ( model
    , Cmd.batch
        [ fetchArticlesForTab shared model
        , GetTags_Home_ |> sendToBackend
        ]
    )


fetchArticlesForTab :
    Shared.Model
    ->
        { model
            | page : Int
            , activeTab : Tab
        }
    -> Cmd Msg
fetchArticlesForTab shared model =
    case model.activeTab of
        Global ->
            ArticleList_Home_
                { filters = Filters.create
                , page = model.page
                }
                |> sendToBackend

        FeedFor user ->
            ArticleFeed_Home_
                { page = model.page
                }
                |> sendToBackend

        TagFilter tag ->
            ArticleList_Home_
                { filters = Filters.create |> Filters.withTag tag
                , page = model.page
                }
                |> sendToBackend



-- UPDATE


type Msg
    = GotArticles (Response Data.Article.Listing)
    | GotTags (Response (List Tag))
    | SelectedTab Tab
    | ClickedFavorite User Article
    | ClickedUnfavorite User Article
    | ClickedPage Int
    | UpdatedArticle (Response Article)


type alias Tag =
    String


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case msg of
        GotArticles listing ->
            ( { model | listing = listing }
            , Cmd.none
            )

        GotTags tags ->
            ( { model | tags = tags }
            , Cmd.none
            )

        SelectedTab tab ->
            let
                newModel : Model
                newModel =
                    { model
                        | activeTab = tab
                        , listing = Data.Response.Loading
                        , page = 1
                    }
            in
            ( newModel
            , fetchArticlesForTab shared newModel
            )

        ClickedFavorite user article ->
            ( model
            , ArticleFavorite_Home_
                { slug = article.slug
                }
                |> sendToBackend
            )

        ClickedUnfavorite user article ->
            ( model
            , ArticleUnfavorite_Home_
                { slug = article.slug
                }
                |> sendToBackend
            )

        ClickedPage page_ ->
            let
                newModel : Model
                newModel =
                    { model
                        | listing = Data.Response.Loading
                        , page = page_
                    }
            in
            ( newModel
            , fetchArticlesForTab shared newModel
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
    { title = ""
    , body =
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
            |> Element.html
            |> Element.el
                [ Element.htmlAttribute <| class "banner"
                , Element.width <| Element.fill
                ]
        , div [ class "row" ]
            [ div [ class "col-md-9" ] <|
                (viewTabs shared model
                    :: View.ArticleList.view
                        { user = shared.user
                        , articleListing = model.listing
                        , onFavorite = ClickedFavorite
                        , onUnfavorite = ClickedUnfavorite
                        , onPageClick = ClickedPage
                        }
                )
            , div [ class "col-md-3" ] [ viewTags model.tags ]
            ]
            |> Element.html
            |> Element.el
                [ Element.htmlAttribute <| class "container page"
                , Element.width <| Element.fill
                ]
        ]
            |> Element.column
                [ Element.htmlAttribute <| class "home-page"
                , Element.width Element.fill
                , Element.height Element.fill
                ]
    }


viewTabs :
    Shared.Model
    -> { model | activeTab : Tab }
    -> Html Msg
viewTabs shared model =
    div [ class "feed-toggle" ]
        [ ul [ class "nav nav-pills outline-active" ]
            [ Utils.Maybe.view shared.user <|
                \user ->
                    li [ class "nav-item" ]
                        [ button
                            [ class "nav-link"
                            , classList [ ( "active", model.activeTab == FeedFor user ) ]
                            , Events.onClick (SelectedTab (FeedFor user))
                            ]
                            [ text "Your Feed" ]
                        ]
            , li [ class "nav-item" ]
                [ button
                    [ class "nav-link"
                    , classList [ ( "active", model.activeTab == Global ) ]
                    , Events.onClick (SelectedTab Global)
                    ]
                    [ text "Global Feed" ]
                ]
            , case model.activeTab of
                TagFilter tag ->
                    li [ class "nav-item" ] [ a [ class "nav-link active" ] [ text ("#" ++ tag) ] ]

                _ ->
                    text ""
            ]
        ]


viewTags : Response (List Tag) -> Html Msg
viewTags response =
    case response of
        Data.Response.Success tags ->
            div [ class "sidebar" ]
                [ p [] [ text "Popular Tags" ]
                , div [ class "tag-list" ] <|
                    List.map
                        (\tag ->
                            button
                                [ class "tag-pill tag-default"
                                , Events.onClick (SelectedTab (TagFilter tag))
                                ]
                                [ text tag ]
                        )
                        tags
                ]

        _ ->
            text ""
