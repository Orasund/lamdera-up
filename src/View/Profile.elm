module View.Profile exposing (list, view)

import Config.View
import Data.Game exposing (Rule)
import Data.Game.Pointer exposing (Pointer)
import Data.Profile exposing (Profile)
import Data.User exposing (User)
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Input as Input
import View.Color as Color
import View.Rule as Rule
import Widget
import Widget.Material as Material
import Widget.Material.Typography as Typography


list :
    { currentUser : Maybe User
    , rules : List Rule
    }
    -> List Profile
    -> Element msg
list { currentUser, rules } =
    List.map
        (\profile ->
            Widget.multiLineItem (Material.multiLineItem Color.palette)
                { title = profile.username
                , text = String.fromInt profile.points ++ " points"
                , onPress = Nothing
                , icon = \_ -> Element.none
                , content = \_ -> Element.none
                }
        )
        >> Widget.itemList Material.column


view :
    { currentUser : Maybe User
    , rules : List Rule
    , amountChanged : String -> msg
    , amountToSpend : String
    , triggeredRule : { rule : Pointer Rule, amountSpent : Int } -> msg
    }
    -> Profile
    -> Element msg
view { currentUser, rules, amountChanged, triggeredRule, amountToSpend } profile =
    let
        isViewingOwnProfile : Bool
        isViewingOwnProfile =
            Maybe.map .username currentUser == Just profile.username

        tokens =
            currentUser
                |> Maybe.map .tokens
                |> Maybe.withDefault 0
    in
    [ [ Element.text profile.username |> Element.el Typography.h2
      , profile.bio |> Maybe.map Element.text |> Maybe.withDefault Element.none
      , Element.text <| "Points: " ++ String.fromInt profile.points
      , if isViewingOwnProfile then
            Element.text <| "Tokens: " ++ String.fromInt tokens

        else
            Element.none
      ]
        |> Element.column
            [ Element.spacing Config.View.spacing
            ]
    , if isViewingOwnProfile then
        (([ Element.text "Actions" |> Element.el Typography.h4
          , [ Element.text "Spend "
            , Input.text
                (Material.textInputAttributes Color.palette
                    ++ [ Element.width <| Element.px 64 ]
                )
                { text = amountToSpend
                , placeholder = Nothing
                , label = "Amount to spend" |> Input.labelHidden
                , onChange = amountChanged
                }
            , Element.text " Token(s)"
            ]
                |> Element.row [ Element.width Element.shrink ]
          ]
            |> Element.row [ Element.spaceEvenly, Element.width Element.fill ]
         )
            :: (rules
                    |> List.map
                        (Rule.view
                            { triggerRule = triggeredRule
                            , amountSpent =
                                amountToSpend
                                    |> String.toInt
                                    |> Maybe.withDefault 0
                            , tokens = tokens
                            }
                        )
               )
        )
            |> Element.column
                [ Element.spacing Config.View.spacing
                , Element.width Element.fill
                ]

      else
        Element.none
    ]
        |> Element.column
            (Material.cardAttributes Color.palette
                ++ [ Border.rounded Config.View.rounded
                   , Element.padding Config.View.padding
                   , Element.spacing (2 * Config.View.spacing)
                   , Element.width <| Element.maximum Config.View.maxWidth <| Element.fill
                   , Element.centerX
                   , Element.centerY
                   ]
            )
