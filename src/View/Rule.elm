module View.Rule exposing (view)

import Config.View
import Data.Game exposing (Rule, Trigger(..))
import Data.Game.Pointer as Pointer exposing (Pointer)
import Element exposing (Element)
import View.Color as Color
import Widget
import Widget.Material as Material


view : { msgMapper : Pointer Rule -> msg, tokens : Int } -> Rule -> Element msg
view args rule =
    case rule.trigger of
        TokenSpend amount ->
            [ Element.text rule.description
            , [ Element.text <| "Costs " ++ String.fromInt amount ++ " Token(s)"
              , Widget.textButton (Material.containedButton Color.palette)
                    { text = "Trigger"
                    , onPress =
                        if args.tokens >= amount then
                            rule.id |> Pointer.fromInt |> args.msgMapper |> Just

                        else
                            Nothing
                    }
              ]
                |> Element.row [ Element.spacing Config.View.spacing ]
            ]
                |> Element.row
                    [ Element.spaceEvenly
                    , Element.width Element.fill
                    ]

        _ ->
            Element.none
