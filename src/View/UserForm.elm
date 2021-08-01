module View.UserForm exposing (Field, view)

import Api.Data exposing (Data)
import Api.User exposing (User)
import Color
import Config.View
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, type_, value)
import Html.Events as Events
import View.Color as Color
import View.ErrorList
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography


type alias Field msg =
    { label : String
    , type_ : String
    , value : String
    , onInput : String -> msg
    }


view :
    { user : Data User
    , label : String
    , onFormSubmit : msg
    , alternateLink : { label : String, route : Route }
    , fields : List (Field msg)
    , msgMapper : Route -> msg
    }
    -> Element msg
view options =
    [ [ Element.text options.label
            |> Element.el
                Typography.h2
      , Widget.textButton (Material.textButton Color.palette)
            { text = options.alternateLink.label
            , onPress = Just <| options.msgMapper <| options.alternateLink.route
            }
            |> Element.el [ Element.centerX ]
      ]
        |> Element.column
            [ Element.spacing 8
            , Element.centerX
            ]
    , options.fields
        |> List.map viewField
        |> Element.column
            [ Element.spacing 16
            ]
    , case options.user of
        Api.Data.Failure reasons ->
            View.ErrorList.view reasons |> Element.html

        _ ->
            Element.text ""
    , Widget.textButton
        (Material.containedButton Color.palette)
        { text = options.label
        , onPress = Just options.onFormSubmit
        }
        |> Element.el [ Element.alignRight ]
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Element.spacing Config.View.spacing
                   , Border.rounded Config.View.rounded
                   , Element.padding Config.View.padding
                   , Element.width Element.shrink
                   , Element.centerX
                   , Element.centerY
                   ]
            )


viewField : Field msg -> Element msg
viewField options =
    case options.type_ of
        "password" ->
            Widget.currentPasswordInputV2 (Material.passwordInput Color.palette)
                { text = options.value
                , placeholder =
                    options.label
                        |> Element.text
                        |> Input.placeholder []
                        |> Just
                , label = options.label
                , onChange = options.onInput
                , show = False
                }

        "email" ->
            Widget.emailInput (Material.textInput Color.palette)
                { chips = []
                , text = options.value
                , placeholder =
                    options.label
                        |> Element.text
                        |> Input.placeholder []
                        |> Just
                , label = options.label
                , onChange = options.onInput
                }

        _ ->
            Widget.textInput (Material.textInput Color.palette)
                { chips = []
                , text = options.value
                , placeholder =
                    options.label
                        |> Element.text
                        |> Input.placeholder []
                        |> Just
                , label = options.label
                , onChange = options.onInput
                }
