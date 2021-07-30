module Pages.NotFound exposing (view)

import Html exposing (..)
import View exposing (View)
import View.NotFound


view : View msg
view =
    { title = "404"
    , body =
        View.NotFound.view
    }
