module View.NotFound exposing (view)

import Config.View
import Element exposing (Element)
import Gen.Route as Route exposing (Route(..))
import Html exposing (..)
import Html.Attributes exposing (class, href)
import View.Color as Color
import Widget
import Widget.Material as Material
import Widget.Material.Typography as Typography


view : (Route -> msg) -> Element msg
view msgMapper =
    [ Element.text "Page not found." |> Element.el Typography.h2
    , Widget.textButton (Material.textButton Color.palette)
        { text = "Home"
        , onPress = Home_ |> msgMapper |> Just
        }
        |> Element.el [ Element.centerX ]
    ]
        |> Element.column
            [ Element.centerX
            , Element.centerY
            , Element.spacing Config.View.spacing
            ]
