module View.DiscussionList exposing (view)

import Data.Discussion exposing (Discussion)
import Data.Response exposing (Response)
import Data.User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, src)
import Html.Events as Events
import Utils.Maybe
import Utils.Time
import View.IconButton as IconButton


view :
    { user : Maybe User
    , discussionListing : Response Data.Discussion.Listing
    , onFavorite : User -> Discussion -> msg
    , onUnfavorite : User -> Discussion -> msg
    , onPageClick : Int -> msg
    }
    -> List (Html msg)
view options =
    case options.discussionListing of
        Data.Response.Loading ->
            [ div [ class "discussion-preview" ] [ text "Loading..." ] ]

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
                [ List.map (viewDiscussionPreview options) listing.discussions
                , [ List.range 1 listing.totalPages
                        |> List.map viewPage
                        |> ul [ class "pagination" ]
                  ]
                ]

        _ ->
            []


viewDiscussionPreview :
    { options
        | user : Maybe User
        , onFavorite : User -> Discussion -> msg
        , onUnfavorite : User -> Discussion -> msg
    }
    -> Discussion
    -> Html msg
viewDiscussionPreview options discussion =
    div [ class "discussion-preview" ]
        [ div [ class "discussion-meta" ]
            [ div [ class "info" ]
                [ a [ class "author", href ("/profile/" ++ discussion.author.username) ] [ text discussion.author.username ]
                , span [ class "date" ] [ text (Utils.Time.formatDate discussion.createdAt) ]
                ]
            , div [ class "pull-xs-right" ]
                [ Utils.Maybe.view options.user <|
                    \user ->
                        if user.username == discussion.author.username then
                            text ""

                        else if discussion.favorited then
                            IconButton.view
                                { color = IconButton.FilledGreen
                                , icon = IconButton.Heart
                                , label = " " ++ String.fromInt discussion.favoritesCount
                                , onClick = options.onUnfavorite user discussion
                                }

                        else
                            IconButton.view
                                { color = IconButton.OutlinedGreen
                                , icon = IconButton.Heart
                                , label = " " ++ String.fromInt discussion.favoritesCount
                                , onClick = options.onFavorite user discussion
                                }
                ]
            ]
        , a [ class "preview-link", href ("/discussion/" ++ discussion.slug) ]
            [ h1 [] [ text discussion.title ]
            , p [] [ text discussion.description ]
            , span [] [ text "Read more..." ]
            , if List.isEmpty discussion.tags then
                text ""

              else
                ul [ class "tag-list" ]
                    (List.map
                        (\tag -> li [ class "tag-default tag-pill tag-outline" ] [ text tag ])
                        discussion.tags
                    )
            ]
        ]
