module View.Widget exposing (..)

import Element exposing (Element)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import View.Color as Color
import Widget
import Widget.Icon as Icon
import Widget.Material as Material


textButton :
    { text : String
    , icon : Icon msg
    , onPress : msg
    }
    -> Element msg
textButton args =
    Widget.button (Material.textButton Color.palette)
        { text = args.text
        , icon = args.icon |> Icon.elmMaterialIcons Color
        , onPress = Just args.onPress
        }


button :
    { text : String
    , icon : Icon msg
    , onPress : Maybe msg
    }
    -> Element msg
button args =
    Widget.button (Material.containedButton Color.palette)
        { text = args.text
        , icon = args.icon |> Icon.elmMaterialIcons Color
        , onPress = args.onPress
        }
