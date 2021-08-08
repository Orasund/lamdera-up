module Pages.Discussion.Slug_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Config.View
import Data.Discussion exposing (Discussion, Slug)
import Data.Discussion.Comment exposing (Comment)
import Data.Profile exposing (Profile)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Effect exposing (Effect)
import Element exposing (Element)
import Element.Border as Border
import Gen.Params.Discussion.Slug_ exposing (Params)
import Gen.Route as Route exposing (Route(..))
import Html exposing (..)
import Page
import Request
import Shared
import Utils.Route
import Utils.Time
import View exposing (View)
import View.Color as Color
import View.DiscussionList
import View.Input
import Widget
import Widget.Material as Material
import Widget.Material.Typography as Typography


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
init _ { params } =
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
            { page = model.page
            }
            |> sendToBackend
        ]
        |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = GotDiscussion (Response Discussion)
    | ClickedDeleteDiscussion User Discussion
    | DeletedDiscussion (Response Discussion)
    | GotAuthor (Response Profile)
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


update : Request.With Params -> Msg -> Model -> ( Model, Effect Msg )
update req msg model =
    case msg of
        GotDiscussion discussion ->
            ( { model | discussion = discussion }
            , Effect.none
            )

        ClickedDeleteDiscussion _ discussion ->
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

        GotComments comments ->
            ( { model | comments = comments }
            , Effect.none
            )

        UpdatedCommentText text ->
            ( { model | commentText = text }
            , Effect.none
            )

        SubmittedCommentForm _ discussion ->
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

        ClickedDeleteComment _ discussion comment ->
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model.discussion of
        Data.Response.Success discussion ->
            { title = discussion.title
            , body =
                [ View.DiscussionList.view
                    { user = shared.user
                    , discussionListing = model.listing
                    , onPageClick = ClickedPage
                    , onClick =
                        \slug ->
                            { slug = slug }
                                |> Discussion__Slug_
                                |> RequestedRouteChange
                    , onNewDiscussion = Route.Editor |> RequestedRouteChange
                    }
                , [ [ Element.text discussion.title |> Element.el Typography.h2
                    , viewDiscussionMeta shared discussion
                    ]
                        |> Element.column
                            [ Element.width <| Element.fill
                            , Element.spacing Config.View.spacing
                            ]
                  , viewCommentSection shared model discussion
                  ]
                    |> Element.column
                        [ Element.width <| Element.maximum Config.View.maxWidth <| Element.fill
                        , Element.centerX
                        , Element.padding Config.View.padding
                        , Element.spacing (2 * Config.View.spacing)
                        ]
                    |> Element.el
                        [ Element.fill |> Element.width
                        , Element.alignTop
                        ]
                ]
                    |> Element.row
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
            }

        _ ->
            { title = "Discussion"
            , body = Element.none
            }


viewDiscussionMeta : Shared.Model -> Discussion -> Element Msg
viewDiscussionMeta shared discussion =
    [ case shared.user of
        Just user ->
            viewControls discussion user

        Nothing ->
            Element.none
    ]
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing Config.View.spacing
            ]


viewControls : Discussion -> User -> Element Msg
viewControls discussion user =
    if discussion.author.username == user.username then
        [ Widget.textButton (Material.textButton Color.palette)
            { text = "Delete"
            , onPress = ClickedDeleteDiscussion user discussion |> Just
            }
        , Widget.textButton (Material.containedButton Color.palette)
            { text = "Edit"
            , onPress =
                Route.Editor__DiscussionSlug_ { discussionSlug = discussion.slug }
                    |> RequestedRouteChange
                    |> Just
            }
        ]
            |> Element.row
                [ Element.alignRight
                , Element.spacing Config.View.spacing
                ]

    else
        Element.none


viewCommentSection : Shared.Model -> Model -> Discussion -> Element Msg
viewCommentSection shared model discussion =
    [ case model.comments of
        Data.Response.Success comments ->
            comments
                |> List.map (viewComment shared.user discussion)
                |> Element.column
                    [ Element.fill
                        |> Element.width
                    , Element.spacing Config.View.spacing
                    ]

        _ ->
            Element.none
    , case shared.user of
        Just user ->
            viewCommentForm model user discussion

        Nothing ->
            Element.none
    ]
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing Config.View.padding
            ]


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
        ownComment : Bool
        ownComment =
            case currentUser of
                Just user ->
                    user.username == comment.author.username

                Nothing ->
                    False

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
    [ [ Widget.textButton (Material.textButton Color.palette)
            { text = comment.author.username
            , onPress =
                Route.Profile__Id_ { id = comment.author.id |> String.fromInt }
                    |> RequestedRouteChange
                    |> Just
            }
      , viewCommentActions
      ]
        |> Element.row [ Element.spaceEvenly, Element.width Element.fill ]
    , comment.body
        |> String.split "\n"
        |> List.map (Element.text >> List.singleton >> Element.paragraph [])
        |> Element.column [ Element.paddingXY Config.View.spacing 0 ]
    , Element.text (Utils.Time.formatDate comment.createdAt)
        |> Element.el [ Element.alignRight ]
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Border.rounded Config.View.rounded
                   , Element.width Element.shrink
                   , if ownComment then
                        Element.alignRight

                     else
                        Element.alignLeft
                   ]
            )
        |> Element.el
            [ if ownComment then
                Element.paddingEach
                    { top = 0
                    , right = 0
                    , bottom = 0
                    , left = Config.View.padding * 2
                    }

              else
                Element.paddingEach
                    { top = 0
                    , right = Config.View.padding * 2
                    , bottom = 0
                    , left = 0
                    }
            , Element.fill
                |> Element.maximum Config.View.maxWidth
                |> Element.width
            ]
