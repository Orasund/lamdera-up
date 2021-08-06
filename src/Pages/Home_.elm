module Pages.Home_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Config.View
import Data.Discussion exposing (Discussion)
import Data.Discussion.Filters as Filters
import Data.Response exposing (Response)
import Data.User exposing (User)
import Effect exposing (Effect)
import Element exposing (Element)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import Page
import Request exposing (Request)
import Shared
import Utils.Maybe
import View exposing (View)
import View.Color as Color
import View.DiscussionList
import Widget
import Widget.Material as Material


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { listing : Response Data.Discussion.Listing
    , page : Int
    , tags : Response (List Tag)
    , activeTab : Tab
    }


type Tab
    = FeedFor User
    | Global
    | TagFilter Tag


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    let
        activeTab : Tab
        activeTab =
            Global

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
        [ fetchDiscussionsForTab shared model
        , GetTags_Home_ |> sendToBackend
        ]
        |> Effect.fromCmd
    )


fetchDiscussionsForTab :
    Shared.Model
    ->
        { model
            | page : Int
            , activeTab : Tab
        }
    -> Cmd Msg
fetchDiscussionsForTab shared model =
    case model.activeTab of
        Global ->
            DiscussionList_Home_
                { filters = Filters.create
                , page = model.page
                }
                |> sendToBackend

        FeedFor user ->
            DiscussionFeed_Home_
                { page = model.page
                }
                |> sendToBackend

        TagFilter tag ->
            DiscussionList_Home_
                { filters = Filters.create |> Filters.withTag tag
                , page = model.page
                }
                |> sendToBackend



-- UPDATE


type Msg
    = GotDiscussions (Response Data.Discussion.Listing)
    | GotTags (Response (List Tag))
    | SelectedTab Tab
    | ClickedFavorite User Discussion
    | ClickedUnfavorite User Discussion
    | ClickedPage Int
    | UpdatedDiscussion (Response Discussion)
    | RequestedRouteChange Route


type alias Tag =
    String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        GotDiscussions listing ->
            ( { model | listing = listing }
            , Effect.none
            )

        GotTags tags ->
            ( { model | tags = tags }
            , Effect.none
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
            , fetchDiscussionsForTab shared newModel |> Effect.fromCmd
            )

        ClickedFavorite user discussion ->
            ( model
            , DiscussionFavorite_Home_
                { slug = discussion.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedUnfavorite user discussion ->
            ( model
            , DiscussionUnfavorite_Home_
                { slug = discussion.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
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
            , fetchDiscussionsForTab shared newModel
                |> Effect.fromCmd
            )

        UpdatedDiscussion (Data.Response.Success discussion) ->
            ( { model
                | listing =
                    Data.Response.map (Data.Discussion.updateDiscussion discussion)
                        model.listing
              }
            , Effect.none
            )

        UpdatedDiscussion _ ->
            ( model, Effect.none )

        RequestedRouteChange route ->
            ( model, Shared.RequestedRouteChange route |> Effect.fromShared )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = ""
    , body =
        [ [ (Widget.textButton (Material.textButton Color.palette)
                { text = "Start Discussion"
                , onPress = Route.Editor |> RequestedRouteChange |> Just
                }
                :: (View.DiscussionList.view
                        { user = shared.user
                        , discussionListing = model.listing
                        , onFavorite = ClickedFavorite
                        , onUnfavorite = ClickedUnfavorite
                        , onPageClick = ClickedPage
                        }
                        |> List.map Element.html
                   )
            )
                |> Element.row []
          , viewTags model.tags
          ]
            |> Element.row
                [ Element.htmlAttribute <| class "container page"
                , Element.width <| Element.fill
                , Element.spaceEvenly
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


viewTags : Response (List Tag) -> Element Msg
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
                |> Element.html

        _ ->
            Element.text ""
