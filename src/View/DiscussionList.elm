module View.DiscussionList exposing (view)

import Config.View
import Data.Discussion exposing (Discussion, Slug)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Element exposing (Element)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import View.Color as Color
import Widget exposing (Item)
import Widget.Material as Material


view :
    { user : Maybe User
    , discussionListing : Response Data.Discussion.Listing
    , onFavorite : User -> Discussion -> msg
    , onUnfavorite : User -> Discussion -> msg
    , onPageClick : Int -> msg
    , onClick : Slug -> msg
    , onNewDiscussion : msg
    }
    -> Element msg
view options =
    let
        list =
            case options.discussionListing of
                Data.Response.Loading ->
                    []

                Data.Response.Success listing ->
                    let
                        viewPage : Int -> Html msg
                        viewPage page =
                            li
                                [ class "page-item"
                                , classList [ ( "active", listing.page == page ) ]
                                ]
                                [ button
                                    [ class "page-link"
                                    , Events.onClick (options.onPageClick page)
                                    ]
                                    [ text (String.fromInt page) ]
                                ]
                    in
                    List.concat
                        [ listing.discussions
                            |> List.map (viewDiscussionPreview options.onClick)
                        , [ List.range 1 listing.totalPages
                                |> List.map viewPage
                                |> ul [ class "pagination" ]
                                |> Element.html
                                |> Element.el [ Element.padding Config.View.padding ]
                                |> Widget.asItem
                          ]
                        ]

                _ ->
                    []
    in
    Widget.fullBleedItem (Material.fullBleedItem Color.palette)
        { text = "New Discussion"
        , icon = always Element.none
        , onPress = Just options.onNewDiscussion
        }
        :: list
        |> Widget.itemList (Material.sideSheet Color.palette)


viewDiscussionPreview :
    (Slug -> msg)
    -> Discussion
    -> Item msg
viewDiscussionPreview onClick discussion =
    Widget.multiLineItem (Material.multiLineItem Color.palette)
        { title = discussion.title
        , text = "By " ++ discussion.author.username
        , onPress = discussion.slug |> onClick |> Just
        , icon = always Element.none
        , content = always Element.none
        }
