module Data.Game exposing (Event(..), Game, triggerEvent, init)

import Array
import Data.Game.Player as Player exposing (Player, PlayerId)
import Data.Game.Pointer as Pointer exposing (Pointer)
import Dict exposing (Dict)
import List.Extra as List


type alias Game =
    { player : Dict Int Player
    , rules : List Rule
    }


init : Game
init =
    { player = Dict.empty
    , rules =
        [ { trigger = EveryWeek, commands = [ AddPoints 1 Player.Current ] }
        , { trigger = TokenSpend 1, commands = [ AddPoints 1 Player.Current ] }
        ]
    }


type Trigger
    = TokenSpend Int
    | EveryWeek


type Command
    = AddPoints Int PlayerId
    | AddTokens Int PlayerId
    | SwapPointsWith PlayerId PlayerId


type alias Rule =
    { trigger : Trigger
    , commands : List Command
    }


type Event
    = RuleBought { amount : Int, current : Pointer Player, rule : Pointer Rule }
    | WeekPassed


triggerEvent : Event -> Game -> Game
triggerEvent event g1 =
    g1.rules
        |> filterRule event
        |> List.foldl
            (\{ commands } g2 ->
                commands
                    |> List.foldl
                        (\command g3 ->
                            g3
                                |> getCurrent event
                                |> List.foldl
                                    (\current g4 ->
                                        applyCommand
                                            { command = command
                                            , current = current
                                            }
                                            g4
                                    )
                                    g3
                        )
                        g2
            )
            g1


filterRule : Event -> List Rule -> List Rule
filterRule event =
    case event of
        RuleBought { amount, rule } ->
            Array.fromList
                >> Pointer.getFromArray rule
                >> Maybe.andThen
                    (\r ->
                        if r.trigger == TokenSpend amount then
                            r |> List.singleton |> Just

                        else
                            Nothing
                    )
                >> Maybe.withDefault []

        WeekPassed ->
            List.filterMap
                (\rule ->
                    if rule.trigger == EveryWeek then
                        Just rule

                    else
                        Nothing
                )


getCurrent : Event -> Game -> List (Pointer Player)
getCurrent trigger game =
    case trigger of
        RuleBought { current } ->
            current |> List.singleton

        WeekPassed ->
            game.player
                |> Player.idsOrderedByPoints


applyCommand : { command : Command, current : Pointer Player } -> Game -> Game
applyCommand args game =
    let
        playerIdsOrderedByPoints =
            Player.idsOrderedByPoints game.player

        getPlayerId playerId =
            playerIdsOrderedByPoints
                |> Player.getPointer { playerId = playerId, current = args.current }
    in
    case args.command of
        AddPoints amount playerId ->
            getPlayerId playerId
                |> Maybe.map
                    (\pointer ->
                        mapPlayer
                            (\player ->
                                { player | points = player.points + amount }
                            )
                            pointer
                            game
                    )
                |> Maybe.withDefault game

        AddTokens amount playerId ->
            getPlayerId playerId
                |> Maybe.map
                    (\pointer ->
                        mapPlayer
                            (\player ->
                                { player | tokens = player.points + amount }
                            )
                            pointer
                            game
                    )
                |> Maybe.withDefault game

        SwapPointsWith pId1 pId2 ->
            let
                pointer1 =
                    getPlayerId pId1

                pointer2 =
                    getPlayerId pId2

                player1 =
                    pointer1
                        |> Maybe.andThen (\p -> game.player |> Pointer.get p)

                player2 =
                    pointer2
                        |> Maybe.andThen (\p -> game.player |> Pointer.get p)

                updateFun player =
                    Maybe.map (\p -> { p | points = player.points })
            in
            Maybe.map4
                (\p1 p2 key1 key2 ->
                    { game
                        | player =
                            game.player
                                |> Pointer.update key1 (updateFun p2)
                                |> Pointer.update key2 (updateFun p1)
                    }
                )
                player1
                player2
                pointer1
                pointer2
                |> Maybe.withDefault game


mapPlayer : (Player -> Player) -> Pointer Player -> Game -> Game
mapPlayer fun pointer game =
    { game
        | player =
            game.player |> Pointer.update pointer (Maybe.map fun)
    }
