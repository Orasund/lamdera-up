module Pages.Home_ exposing (Model, Msg(..), page)

import Bridge exposing (..)
import Data.Discussion exposing (Discussion)
import Data.Response exposing (Response)
import Data.User
import Effect exposing (Effect)
import Element
import Gen.Route as Route exposing (Route(..))
import Html exposing (..)
import Html.Attributes exposing (class)
import Page
import Request exposing (Request)
import Shared
import View exposing (View)
import View.DiscussionList


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
    }


init : Shared.Model -> ( Model, Effect Msg )
init _ =
    let
        model : Model
        model =
            { listing = Data.Response.Loading
            , page = 1
            }
    in
    ( model
    , DiscussionList_Home_
        { page = model.page
        }
        |> sendToBackend
        |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = GotDiscussions (Response Data.Discussion.Listing)
    | ClickedPage Int
    | UpdatedDiscussion (Response Discussion)
    | RequestedRouteChange Route


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
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
    { title = ""
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
        , [ []
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
        ]
            |> Element.row
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
    }
