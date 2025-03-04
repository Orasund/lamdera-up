module View exposing (View, map, none, placeholder, toBrowserDocument)

import Element exposing (Element)


type alias View msg =
    { title : String
    , body : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = Element.text str
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = Element.map fn view.body
    }


toBrowserDocument view =
    { title = view.title
    , body = view.body |> Element.layout [] |> List.singleton
    }
