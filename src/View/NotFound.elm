module View.NotFound exposing (view)

import Element exposing (Element)
import Html exposing (..)
import Html.Attributes exposing (class, href)


view : Element msg
view =
    div [ class "container page" ]
        [ h2 [] [ text "Page not found." ]
        , h5 []
            [ text "But here's the "
            , a [ href "/" ] [ text "homepage" ]
            , text "!"
            ]
        ]
        |> Element.html
