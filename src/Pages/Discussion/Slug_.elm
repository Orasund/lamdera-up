module Pages.Discussion.Slug_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Config.View
import Data.Discussion exposing (Discussion, Slug)
import Data.Discussion.Comment exposing (Comment)
import Data.Discussion.Filters as Filters
import Data.Profile exposing (Profile)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Effect exposing (Effect)
import Element exposing (Element)
import Gen.Params.Discussion.Slug_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Markdown
import Page
import Request
import Shared
import Utils.Route
import Utils.Time
import View exposing (View)
import View.Color as Color
import View.DiscussionList
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
    { discussion : Response Discussion
    , comments : Response (List Comment)
    , commentText : String
    , listing : Response Data.Discussion.Listing
    , page : Int
    }


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init shared { params } =
    let
        model : Model
        model =
            { discussion = Data.Response.Loading
            , comments = Data.Response.Loading
            , commentText = ""
            , listing = Data.Response.Loading
            , page = 1
            }
    in
    ( model
    , Cmd.batch
        [ DiscussionGet_Discussion__Slug_
            { slug = params.slug
            }
            |> sendToBackend
        , DiscussionCommentGet_Discussion__Slug_
            { discussionSlug = params.slug
            }
            |> sendToBackend
        , DiscussionList_Discussion__Slug_
            { filters = Filters.create
            , page = model.page
            }
            |> sendToBackend
        ]
        |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = GotDiscussion (Response Discussion)
    | ClickedFavorite User Discussion
    | ClickedUnfavorite User Discussion
    | ClickedDeleteDiscussion User Discussion
    | DeletedDiscussion (Response Discussion)
    | GotAuthor (Response Profile)
    | ClickedFollow User Profile
    | ClickedUnfollow User Profile
    | GotComments (Response (List Comment))
    | ClickedDeleteComment User Discussion Comment
    | DeletedComment (Response Int)
    | SubmittedCommentForm User Discussion
    | CreatedComment (Response Comment)
    | UpdatedCommentText String
    | GotDiscussions (Response Data.Discussion.Listing)
    | ClickedPage Int
    | UpdatedDiscussion (Response Discussion)
    | RequestedRouteChange Route
    | LoadDiscussion Slug


update : Request.With Params -> Msg -> Model -> ( Model, Effect Msg )
update req msg model =
    case msg of
        GotDiscussion discussion ->
            ( { model | discussion = discussion }
            , Effect.none
            )

        ClickedFavorite user discussion ->
            ( model
            , DiscussionFavorite_Discussion__Slug_
                { slug = discussion.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedUnfavorite user discussion ->
            ( model
            , DiscussionUnfavorite_Discussion__Slug_
                { slug = discussion.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedDeleteDiscussion user discussion ->
            ( model
            , DiscussionDelete_Discussion__Slug_
                { slug = discussion.slug
                }
                |> sendToBackend
                |> Effect.fromCmd
            )

        DeletedDiscussion _ ->
            ( model
            , Utils.Route.navigate req.key Route.Home_
                |> Effect.fromCmd
            )

        GotAuthor profile ->
            let
                updateAuthor : Discussion -> Discussion
                updateAuthor discussion =
                    case profile of
                        Data.Response.Success author ->
                            { discussion | author = author }

                        _ ->
                            discussion
            in
            ( { model | discussion = Data.Response.map updateAuthor model.discussion }
            , Effect.none
            )

        ClickedFollow user profile ->
            ( model
            , ProfileFollow_Discussion__Slug_ { id = profile.id }
                |> sendToBackend
                |> Effect.fromCmd
            )

        ClickedUnfollow user profile ->
            ( model
            , ProfileUnfollow_Discussion__Slug_ { id = profile.id }
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

        SubmittedCommentForm user discussion ->
            if String.isEmpty model.commentText then
                ( model, Effect.none )

            else
                ( { model | commentText = "" }
                , DiscussionCommentCreate_Discussion__Slug_
                    { discussionSlug = discussion.slug
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

        ClickedDeleteComment user discussion comment ->
            ( model
            , DiscussionCommentDelete_Discussion__Slug_
                { discussionSlug = discussion.slug
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

        GotDiscussions listing ->
            ( { model | listing = listing }
            , Effect.none
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
            , Effect.none
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

        LoadDiscussion slug ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model.discussion of
        Data.Response.Success discussion ->
            { title = discussion.title
            , body = viewDiscussion shared model discussion
            }

        _ ->
            { title = "Discussion"
            , body = Element.none
            }


viewDiscussion : Shared.Model -> Model -> Discussion -> Element Msg
viewDiscussion shared model discussion =
    [ View.DiscussionList.view
        { user = shared.user
        , discussionListing = model.listing
        , onFavorite = ClickedFavorite
        , onUnfavorite = ClickedUnfavorite
        , onPageClick = ClickedPage
        , onClick = LoadDiscussion
        , onNewDiscussion = Route.Editor |> RequestedRouteChange
        }
    , [ div [ class "container" ]
            [ h1 [] [ text discussion.title ]
            , viewDiscussionMeta shared discussion
            ]
            |> Element.html
            |> Element.el
                [ Element.width <| Element.fill
                , Element.htmlAttribute <| class "banner"
                ]
      , [ div [ class "row discussion-content" ]
            [ div [ class "col-md-12" ]
                [ Markdown.toHtml [] discussion.body ]
            , if List.isEmpty discussion.tags then
                text ""

              else
                ul [ class "tag-list" ]
                    (List.map
                        (\tag -> li [ class "tag-default tag-pill tag-outline" ] [ text tag ])
                        discussion.tags
                    )
            ]
            |> Element.html
        , hr [] [] |> Element.html
        , div [ class "discussion-actions" ] [ viewDiscussionMeta shared discussion ]
            |> Element.html
        , viewCommentSection shared model discussion
        ]
            |> Element.column
                [ class "container page" |> Element.htmlAttribute
                , Element.width <| Element.fill
                ]
      ]
        |> Element.column
            [ Element.width <| Element.fill
            , class "discussion-page" |> Element.htmlAttribute
            ]
    ]
        |> Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            ]


viewDiscussionMeta : Shared.Model -> Discussion -> Html Msg
viewDiscussionMeta shared discussion =
    div [ class "discussion-meta" ] <|
        List.concat
            [ [ div [ class "info" ]
                    [ a [ class "author", href ("/profile/" ++ discussion.author.username) ] [ text discussion.author.username ]
                    , span [ class "date" ] [ text (Utils.Time.formatDate discussion.createdAt) ]
                    ]
              ]
            , case shared.user of
                Just user ->
                    viewControls discussion user

                Nothing ->
                    []
            ]


viewControls : Discussion -> User -> List (Html Msg)
viewControls discussion user =
    if discussion.author.username == user.username then
        [ a
            [ class "btn btn-outline-secondary btn-sm"
            , href ("/editor/" ++ discussion.slug)
            ]
            [ i [ class "ion-edit" ] []
            , text "Edit discussion"
            ]
        , IconButton.view
            { color = IconButton.OutlinedRed
            , icon = IconButton.Trash
            , label = "Delete discussion"
            , onClick = ClickedDeleteDiscussion user discussion
            }
        ]

    else
        [ if discussion.author.following then
            IconButton.view
                { color = IconButton.FilledGray
                , icon = IconButton.Plus
                , label = "Unfollow " ++ discussion.author.username
                , onClick = ClickedUnfollow user discussion.author
                }

          else
            IconButton.view
                { color = IconButton.OutlinedGray
                , icon = IconButton.Plus
                , label = "Follow " ++ discussion.author.username
                , onClick = ClickedFollow user discussion.author
                }
        , if discussion.favorited then
            IconButton.view
                { color = IconButton.FilledGreen
                , icon = IconButton.Heart
                , label = "Unfavorite Post (" ++ String.fromInt discussion.favoritesCount ++ ")"
                , onClick = ClickedUnfavorite user discussion
                }

          else
            IconButton.view
                { color = IconButton.OutlinedGreen
                , icon = IconButton.Heart
                , label = "Favorite Post (" ++ String.fromInt discussion.favoritesCount ++ ")"
                , onClick = ClickedFavorite user discussion
                }
        ]


viewCommentSection : Shared.Model -> Model -> Discussion -> Element Msg
viewCommentSection shared model discussion =
    [ case model.comments of
        Data.Response.Success comments ->
            List.map (viewComment shared.user discussion) comments
                |> Element.column
                    [ Element.fill
                        |> Element.maximum Config.View.maxWidth
                        |> Element.width
                    ]

        _ ->
            Element.none
    , case shared.user of
        Just user ->
            viewCommentForm model user discussion

        Nothing ->
            Element.none
    ]
        |> Element.column [ Element.width Element.fill ]


viewCommentForm : Model -> User -> Discussion -> Element Msg
viewCommentForm model user discussion =
    [ View.Input.multiLineInput
        { onChange = UpdatedCommentText
        , text = model.commentText
        , label = "Write a comment..."
        }
    , Widget.textButton (Material.containedButton Color.palette)
        { text = "Post Comment"
        , onPress = Just <| SubmittedCommentForm user discussion
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


viewComment : Maybe User -> Discussion -> Comment -> Element Msg
viewComment currentUser discussion comment =
    let
        viewCommentActions : Element Msg
        viewCommentActions =
            case currentUser of
                Just user ->
                    if user.username == comment.author.username then
                        Widget.textButton (Material.textButton Color.palette)
                            { text = "Delete"
                            , onPress = Just <| ClickedDeleteComment user discussion comment
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
                Route.Profile__Id_ { id = comment.author.id |> String.fromInt }
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
