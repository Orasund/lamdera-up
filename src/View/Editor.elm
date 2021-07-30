module View.Editor exposing
    ( Field
    , Form
    , updateField
    , view
    )

import Api.Article exposing (Article)
import Api.Data exposing (Data)
import Element exposing (Element)
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events as Events
import View.Color as Color
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


textInput : { label : String, text : String, onChange : String -> msg } -> Element msg
textInput args =
    Widget.textInput
        (Material.textInput Color.palette
            |> Customize.elementRow [ Element.width Element.fill ]
        )
        { chips = []
        , text = args.text
        , placeholder =
            Element.text args.label
                |> Input.placeholder []
                |> Just
        , label = args.label
        , onChange = args.onChange
        }


multiLineInput : { label : String, text : String, onChange : String -> msg } -> Element msg
multiLineInput args =
    Input.multiline
        ((Material.textInput Color.palette).elementRow
            ++ [ Element.width Element.fill
               , Element.padding 16
               , Element.height <| Element.px 256
               ]
        )
        { onChange = args.onChange
        , text = args.text
        , placeholder =
            Element.text args.label
                |> Input.placeholder []
                |> Just
        , label =
            args.label
                |> Input.labelHidden
        , spellcheck = True
        }


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
    , article : Data Article
    }
    -> Element msg
view options =
    [ Element.text options.title |> Element.el Typography.h2
    , [ textInput
            { text = options.form.title
            , label = "Article Title"
            , onChange = options.onUpdate Title
            }
      , textInput
            { text = options.form.description
            , label = "What's this article about?"
            , onChange = options.onUpdate Description
            }
      , multiLineInput
            { onChange = options.onUpdate Body
            , text = options.form.body
            , label = "Write your article (in markdown)"
            }
      , textInput
            { text = options.form.tags
            , label = "Enter tags (separated by commas)"
            , onChange = options.onUpdate Tags
            }
      ]
        |> Element.column
            [ Element.spacing 16
            , Element.width Element.fill
            ]
    , Widget.textButton (Material.containedButton Color.palette)
        { text = options.buttonLabel
        , onPress = Just options.onFormSubmit
        }
    , case options.article of
        Api.Data.Failure reasons ->
            ul [ class "error-messages" ]
                (List.map (\message -> li [] [ text message ]) reasons)
                |> Element.html

        _ ->
            Element.text ""
    ]
        |> Element.column
            [ Element.centerY
            , Element.centerX
            , Element.spacing 32
            , Element.width <| Element.maximum 1024 <| Element.fill
            , Element.paddingXY 64 0
            ]
