module View.Navbar exposing (view)

import Array
import Data.User exposing (User)
import Element exposing (Element)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
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
                    [ ( "Profile", Route.Profile__Id_ { id = user.id |> String.fromInt } )
                    ]

                Nothing ->
                    []

        selected =
            menu
                |> List.indexedMap Tuple.pair
                |> List.filterMap
                    (\( i, ( _, route ) ) ->
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
        { title = Element.none
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
