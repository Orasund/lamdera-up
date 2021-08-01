module View.Input exposing (multiLineInput, textInput, withLabel)

import Config.View
import Element exposing (Element)
import Element.Input as Input
import View.Color as Color
import Widget
import Widget.Customize as Customize
import Widget.Material as Material


withLabel : String -> Element msg -> Element msg
withLabel spring inputs =
    [ Element.text spring
        |> Element.el [ Element.alignRight ]
        |> Element.el
            [ Element.width Element.fill
            , Element.alignTop
            , Element.paddingXY 0 12
            ]
    , inputs
    , Element.none |> Element.el [ Element.width Element.fill ]
    ]
        |> Element.row [ Element.width Element.fill, Element.spacing Config.View.spacing ]


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
        (Material.textInputAttributes Color.palette
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
