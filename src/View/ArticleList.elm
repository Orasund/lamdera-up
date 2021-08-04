module View.ArticleList exposing (view)

import Data.Article exposing (Article)
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
    , articleListing : Response Data.Article.Listing
    , onFavorite : User -> Article -> msg
    , onUnfavorite : User -> Article -> msg
    , onPageClick : Int -> msg
    }
    -> List (Html msg)
view options =
    case options.articleListing of
        Data.Response.Loading ->
            [ div [ class "article-preview" ] [ text "Loading..." ] ]

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
                [ List.map (viewArticlePreview options) listing.articles
                , [ List.range 1 listing.totalPages
                        |> List.map viewPage
                        |> ul [ class "pagination" ]
                  ]
                ]

        _ ->
            []


viewArticlePreview :
    { options
        | user : Maybe User
        , onFavorite : User -> Article -> msg
        , onUnfavorite : User -> Article -> msg
    }
    -> Article
    -> Html msg
viewArticlePreview options article =
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ div [ class "info" ]
                [ a [ class "author", href ("/profile/" ++ article.author.username) ] [ text article.author.username ]
                , span [ class "date" ] [ text (Utils.Time.formatDate article.createdAt) ]
                ]
            , div [ class "pull-xs-right" ]
                [ Utils.Maybe.view options.user <|
                    \user ->
                        if user.username == article.author.username then
                            text ""

                        else if article.favorited then
                            IconButton.view
                                { color = IconButton.FilledGreen
                                , icon = IconButton.Heart
                                , label = " " ++ String.fromInt article.favoritesCount
                                , onClick = options.onUnfavorite user article
                                }

                        else
                            IconButton.view
                                { color = IconButton.OutlinedGreen
                                , icon = IconButton.Heart
                                , label = " " ++ String.fromInt article.favoritesCount
                                , onClick = options.onFavorite user article
                                }
                ]
            ]
        , a [ class "preview-link", href ("/article/" ++ article.slug) ]
            [ h1 [] [ text article.title ]
            , p [] [ text article.description ]
            , span [] [ text "Read more..." ]
            , if List.isEmpty article.tags then
                text ""

              else
                ul [ class "tag-list" ]
                    (List.map
                        (\tag -> li [ class "tag-default tag-pill tag-outline" ] [ text tag ])
                        article.tags
                    )
            ]
        ]
