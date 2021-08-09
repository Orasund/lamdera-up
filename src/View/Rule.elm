module View.Rule exposing (view)

import Config.View
import Data.Game exposing (Rule, Trigger(..))
import Data.Game.Pointer as Pointer exposing (Pointer)
import Element exposing (Element)
import View.Color as Color
import Widget
import Widget.Material as Material


view :
    { triggerRule : { rule : Pointer Rule, amountSpent : Int } -> msg
    , amountSpent : Int
    , tokens : Int
    }
    -> Rule
    -> Element msg
view args rule =
    let
        fun amount string =
            [ rule.description
                |> Element.text
                |> List.singleton
                |> Element.paragraph
                    [ Element.paddingEach
                        { top = 0
                        , right = Config.View.spacing
                        , bottom = 0
                        , left = 0
                        }
                    ]
            , [ Element.text <| string
              , Widget.textButton (Material.containedButton Color.palette)
                    { text = "Trigger"
                    , onPress =
                        if (args.tokens >= amount) && (args.amountSpent >= amount) then
                            { rule = rule.id |> Pointer.fromInt
                            , amountSpent = amount
                            }
                                |> args.triggerRule
                                |> Just

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
    in
    case rule.trigger of
        TokensSpend amount ->
            fun amount ("Costs " ++ String.fromInt amount ++ " Token(s)")

        SomeTokensSpend ->
            fun args.amountSpent ("Spend  " ++ String.fromInt args.amountSpent ++ " Token(s)")

        _ ->
            Element.none
