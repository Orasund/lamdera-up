module View.Editor exposing
    ( Field
    , Form
    , updateField
    , view
    )

import Config.View
import Data.Discussion exposing (Discussion)
import Data.Response exposing (Response)
import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events as Events
import View.Color as Color
import View.Input
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography


type Field
    = Title
    | Description
    | Body
    | Tags


type alias Form =
    { title : String
    , description : String
    , body : String
    , tags : String
    }


updateField : Field -> String -> Form -> Form
updateField field value form =
    case field of
        Title ->
            { form | title = value }

        Description ->
            { form | description = value }

        Body ->
            { form | body = value }

        Tags ->
            { form | tags = value }


view :
    { onFormSubmit : msg
    , title : String
    , form :
        { title : String
        , description : String
        , body : String
        , tags : String
        }
    , buttonLabel : String
    , onUpdate : Field -> String -> msg
    , discussion : Response Discussion
    }
    -> Element msg
view options =
    [ Element.text options.title |> Element.el Typography.h2
    , [ View.Input.textInput
            { text = options.form.title
            , label = "Discussion Title"
            , onChange = options.onUpdate Title
            }
      , View.Input.textInput
            { text = options.form.description
            , label = "What's this discussion about?"
            , onChange = options.onUpdate Description
            }
      , View.Input.multiLineInput
            { onChange = options.onUpdate Body
            , text = options.form.body
            , label = "Write your discussion (in markdown)"
            }
      , View.Input.textInput
            { text = options.form.tags
            , label = "Enter tags (separated by commas)"
            , onChange = options.onUpdate Tags
            }
      ]
        |> Element.column
            [ Element.spacing Config.View.spacing
            , Element.width Element.fill
            ]
    , Widget.textButton (Material.containedButton Color.palette)
        { text = options.buttonLabel
        , onPress = Just options.onFormSubmit
        }
        |> Element.el [ Element.alignRight ]
    , case options.discussion of
        Data.Response.Failure reasons ->
            ul [ class "error-messages" ]
                (List.map (\message -> li [] [ text message ]) reasons)
                |> Element.html

        _ ->
            Element.text ""
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Element.spacing Config.View.spacing
                   , Border.rounded Config.View.rounded
                   , Element.padding Config.View.padding
                   , Element.width Element.shrink
                   , Element.centerX
                   , Element.centerY
                   , Element.fill
                        |> Element.maximum Config.View.maxWidth
                        |> Element.width
                   ]
            )
