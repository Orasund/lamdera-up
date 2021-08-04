module View.Navbar exposing (view)

import Array
import Data.User exposing (User)
import Dict
import Element exposing (Element)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events as Events
import View.Color as Color
import Widget
import Widget.Material as Material
import Widget.Material.Typography as Typography


view :
    { user : Maybe User
    , currentRoute : Route
    , onSignOut : msg
    , msgMapper : Route -> msg
    }
    -> Element msg
view options =
    let
        defaultItem =
            ( "Home", Route.Home_ )

        menu =
            case options.user of
                Just user ->
                    [ ( "New Article", Route.Editor )
                    , ( "Profil", Route.Profile__Username_ { username = user.username } )
                    ]

                Nothing ->
                    []

        selected =
            menu
                |> List.indexedMap Tuple.pair
                |> List.filterMap
                    (\( i, ( string, route ) ) ->
                        if route == options.currentRoute then
                            Just <| i + 1

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault 0
                |> Just
    in
    Widget.tabBar (Material.tabBar Color.palette)
        { title =
            Element.text "Conduit"
                |> Element.el Typography.h6
        , menu =
            { selected = selected
            , options =
                defaultItem
                    :: menu
                    |> List.map
                        (\( text, _ ) ->
                            { text = text
                            , icon = always Element.none
                            }
                        )
            , onSelect =
                \i ->
                    (if i == 0 || (i > (menu |> List.length)) then
                        defaultItem

                     else
                        menu
                            |> Array.fromList
                            |> Array.get (i - 1)
                            |> Maybe.withDefault defaultItem
                    )
                        |> Tuple.second
                        |> options.msgMapper
                        |> Just
            }
        , deviceClass = Element.Tablet
        , openRightSheet = Nothing
        , openTopSheet = Nothing
        , primaryActions =
            case options.user of
                Just _ ->
                    [ { text = "Settings"
                      , icon = always Element.none
                      , onPress = Just <| options.msgMapper Route.Settings
                      }
                    , { text = "Sign out"
                      , icon = always Element.none
                      , onPress = Just <| options.onSignOut
                      }
                    ]

                Nothing ->
                    [ { text = "Sign in"
                      , icon = always Element.none
                      , onPress = Just <| options.msgMapper Route.Login
                      }
                    , { text = "Sign up"
                      , icon = always Element.none
                      , onPress = Just <| options.msgMapper Route.Register
                      }
                    ]
        , search = Nothing
        }


viewLink : Route -> ( String, Route ) -> Html msg
viewLink currentRoute ( label, route ) =
    li [ class "nav-item" ]
        [ a
            [ class "nav-link"
            , classList [ ( "active", currentRoute == route ) ]
            , href (Route.toHref route)
            ]
            [ text label ]
        ]
