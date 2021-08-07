module View.Rule exposing (view)

import Data.Game exposing (Rule, Trigger(..))
import Data.Game.Pointer as Pointer exposing (Pointer)
import Element exposing (Element)
import View.Color as Color
import Widget
import Widget.Material as Material


view : (Pointer Rule -> msg) -> Rule -> Element msg
view msgMapper rule =
    case rule.trigger of
        TokenSpend amount ->
            [ Element.text rule.description
            , Widget.textButton (Material.containedButton Color.palette)
                { text = "Trigger for " ++ String.fromInt amount ++ " Token(s)"
                , onPress = rule.id |> Pointer.fromInt |> msgMapper |> Just
                }
            ]
                |> Element.column [ Element.spaceEvenly ]

        _ ->
            Element.none
