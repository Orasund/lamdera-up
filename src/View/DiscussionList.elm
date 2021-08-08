module View.DiscussionList exposing (view)

import Config.View
import Data.Discussion exposing (Discussion, Slug)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Element exposing (Element)
import Html exposing (..)
import View.Color as Color
import Widget exposing (Item)
import Widget.Material as Material


view :
    { user : Maybe User
    , discussionListing : Response Data.Discussion.Listing
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
                    List.concat
                        [ [ Widget.select
                                { selected = Just <| listing.page
                                , options =
                                    List.range 1 listing.totalPages
                                        |> List.map
                                            (\int ->
                                                { text = String.fromInt int
                                                , icon = always Element.none
                                                }
                                            )
                                , onSelect = \i -> Just <| options.onPageClick i
                                }
                                |> Widget.buttonRow
                                    { elementRow = Material.buttonRow
                                    , content = Material.toggleButton Material.defaultPalette
                                    }
                                |> Element.el
                                    [ Element.paddingXY Config.View.padding Config.View.spacing
                                    , Element.centerX
                                    ]
                                |> Widget.asItem
                          ]
                        , listing.discussions
                            |> List.map (viewDiscussionPreview options.onClick)
                        ]

                _ ->
                    []
    in
    [ Widget.fullBleedItem (Material.fullBleedItem Color.palette)
        { text = "New Discussion"
        , icon = always Element.none
        , onPress = Just options.onNewDiscussion
        }
    , Widget.divider (Material.fullBleedDivider Color.palette)
    ]
        ++ list
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
