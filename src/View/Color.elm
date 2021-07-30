module View.Color exposing (palette)

import Color
import Widget.Material as Material exposing (Palette)


palette : Palette
palette =
    let
        p =
            Material.defaultPalette
    in
    { p | primary = Color.rgb255 92 184 92 }
