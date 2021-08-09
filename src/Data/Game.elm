module Data.Game exposing
    ( Event(..)
    , Game
    , Rule
    , Trigger(..)
    , addPlayer
    , init
    , spendToken
    , triggerEvent
    )

import Data.Game.IntExp as IntExp exposing (IntExp(..))
import Data.Game.Player as Player exposing (Player, PlayerId)
import Data.Game.Pointer as Pointer exposing (Pointer)
import Dict exposing (Dict)
import List.Extra as List
import Random exposing (Generator)


type alias Game =
    { players : Dict Int Player
    , rules : List Rule
    , hasWon : Maybe (Pointer Player)
    , nextPlayerId : Int
    }


type Trigger
    = TokensSpend Int
    | SomeTokensSpend
    | EveryWeek Int
    | EveryDay


type Command
    = AddPoints IntExp PlayerId
    | AddTokens IntExp PlayerId
    | SwapPointsWith PlayerId PlayerId
    | Win PlayerId


type alias Rule =
    { id : Int
    , description : String
    , trigger : Trigger
    , commands : List Command
    }


type Event
    = RuleBought { current : Pointer Player, rule : Pointer Rule, amountSpent : Int }
    | DayPassed Int


init : Game
init =
    { players = Dict.empty
    , rules =
        [ { id = 1
          , trigger = EveryDay
          , description = "You get a token"
          , commands = [ AddTokens (IntValue 1) Player.Current ]
          }
        , { id = 2
          , trigger = TokensSpend 1
          , description = "You get a random amount of points"
          , commands = [ AddPoints (IntRandom (IntValue 1) (IntValue 6)) Player.Current ]
          }
        , { id = 3
          , trigger = EveryWeek 4
          , description = "The player with the highest points wins"
          , commands = [ Win Player.Highest ]
          }
        , { id = 4
          , trigger = SomeTokensSpend
          , description = "You swap your points with the next N-th highest player, where N is the amount of tokens you spend"
          , commands = [ SwapPointsWith (Player.Relative 1) Player.Current ]
          }
        ]
    , hasWon = Nothing
    , nextPlayerId = 0
    }


addPlayer : Player -> Game -> ( Pointer Player, Game )
addPlayer player game =
    ( game.nextPlayerId |> Pointer.fromInt
    , { game
        | players = game.players |> Dict.insert game.nextPlayerId player
        , nextPlayerId = game.nextPlayerId + 1
      }
    )


spendToken : { rule : Pointer Rule, current : Pointer Player, amountSpent : Int } -> Game -> Generator (Result String Game)
spendToken args game =
    let
        isValid =
            case game.rules |> Pointer.find .id args.rule |> Maybe.map .trigger of
                Just (TokensSpend _) ->
                    True

                Just SomeTokensSpend ->
                    True

                _ ->
                    False
    in
    if isValid then
        (game.players |> Pointer.get args.current)
            |> Maybe.andThen
                (\player ->
                    let
                        tokens =
                            player.tokens - args.amountSpent
                    in
                    if tokens >= 0 then
                        Just { player | tokens = tokens }

                    else
                        Nothing
                )
            |> Maybe.map
                (\player ->
                    { game
                        | players =
                            game.players
                                |> Pointer.update args.current (Maybe.map (always player))
                    }
                        |> triggerEvent
                            (RuleBought
                                { current = args.current
                                , rule = args.rule
                                , amountSpent = args.amountSpent
                                }
                            )
                )
            |> Maybe.withDefault (Random.constant (Ok game))

    else
        Random.constant (Ok game)


triggerEvent : Event -> Game -> Generator (Result String Game)
triggerEvent event g1 =
    let
        amountSpent =
            case event of
                RuleBought a ->
                    a.amountSpent

                _ ->
                    0

        foldCommands : Game -> List Command -> Generator (Result String Game)
        foldCommands =
            foldlList
                (\command result ->
                    case result of
                        Ok g3 ->
                            g3
                                |> getCurrent event
                                |> foldPlayers command g3

                        Err err ->
                            Random.constant <| Err err
                )

        foldPlayers : Command -> Game -> List (Pointer Player) -> Generator (Result String Game)
        foldPlayers command =
            foldlList
                (\current ->
                    Result.andThen
                        (applyCommand
                            { command = command
                            , current = current
                            , amountSpent = amountSpent
                            }
                        )
                        >> resultRandMap
                )

        foldlList :
            (a -> Result String Game -> Generator (Result String Game))
            -> Game
            -> List a
            -> Generator (Result String Game)
        foldlList fun g =
            List.foldl
                (\current ->
                    Random.andThen (fun current)
                )
                (Random.constant <| Ok <| g)
    in
    g1.rules
        |> filterRule event
        |> foldlList
            (\{ commands } result ->
                case result of
                    Ok g2 ->
                        commands
                            |> foldCommands g2

                    Err err ->
                        Random.constant (Err err)
            )
            g1


filterRule : Event -> List Rule -> List Rule
filterRule event =
    case event of
        RuleBought { rule } ->
            Pointer.find .id rule
                >> Maybe.andThen
                    (\r ->
                        case r.trigger of
                            TokensSpend _ ->
                                r |> List.singleton |> Just

                            SomeTokensSpend ->
                                r |> List.singleton |> Just

                            _ ->
                                Nothing
                    )
                >> Maybe.withDefault []

        DayPassed n ->
            List.filterMap
                (\rule ->
                    case rule.trigger of
                        EveryWeek k ->
                            if n |> modBy (7 * k) |> (==) 0 then
                                Just rule

                            else
                                Nothing

                        EveryDay ->
                            Just rule

                        _ ->
                            Nothing
                )


getCurrent : Event -> Game -> List (Pointer Player)
getCurrent trigger game =
    case trigger of
        RuleBought { current } ->
            current |> List.singleton

        DayPassed _ ->
            game.players
                |> Player.idsOrderedByPoints


applyCommand :
    { command : Command, current : Pointer Player, amountSpent : Int }
    -> Game
    -> Result String (Generator Game)
applyCommand args game =
    let
        playerIdsOrderedByPoints =
            Player.idsOrderedByPoints game.players

        getPlayerId playerId =
            playerIdsOrderedByPoints
                |> Player.getPointer { playerId = playerId, current = args.current }
    in
    if game.hasWon /= Nothing then
        Err <| "The game is over"

    else
        case args.command of
            AddPoints amount playerId ->
                getPlayerId playerId
                    |> Maybe.map
                        (\pointer ->
                            IntExp.evaluate amount args.amountSpent
                                |> Random.map
                                    (\int ->
                                        mapPlayer
                                            (\player ->
                                                { player
                                                    | points = player.points + int
                                                }
                                            )
                                            pointer
                                            game
                                    )
                                |> Ok
                        )
                    |> Maybe.withDefault
                        ("could not find "
                            ++ Player.toString playerId
                            |> Err
                        )

            AddTokens amount playerId ->
                getPlayerId playerId
                    |> Maybe.map
                        (\pointer ->
                            IntExp.evaluate amount args.amountSpent
                                |> Random.map
                                    (\int ->
                                        mapPlayer
                                            (\player ->
                                                { player
                                                    | tokens = player.points + int
                                                }
                                            )
                                            pointer
                                            game
                                    )
                                |> Ok
                        )
                    |> Maybe.withDefault
                        ("could not find "
                            ++ Player.toString playerId
                            |> Err
                        )

            SwapPointsWith pId1 pId2 ->
                let
                    pointer1 =
                        getPlayerId pId1

                    pointer2 =
                        getPlayerId pId2

                    player1 =
                        pointer1
                            |> Maybe.andThen (\p -> game.players |> Pointer.get p)

                    player2 =
                        pointer2
                            |> Maybe.andThen (\p -> game.players |> Pointer.get p)

                    updateFun player =
                        Maybe.map (\p -> { p | points = player.points })
                in
                Maybe.map4
                    (\p1 p2 key1 key2 ->
                        { game
                            | players =
                                game.players
                                    |> Pointer.update key1 (updateFun p2)
                                    |> Pointer.update key2 (updateFun p1)
                        }
                            |> Random.constant
                            |> Ok
                    )
                    player1
                    player2
                    pointer1
                    pointer2
                    |> Maybe.withDefault
                        ("could not find either "
                            ++ Player.toString pId1
                            ++ " or "
                            ++ Player.toString pId2
                            |> Err
                        )

            Win playerId ->
                { game | hasWon = getPlayerId playerId }
                    |> Random.constant
                    |> Ok


mapPlayer : (Player -> Player) -> Pointer Player -> Game -> Game
mapPlayer fun pointer game =
    { game
        | players =
            game.players |> Pointer.update pointer (Maybe.map fun)
    }


resultRandMap : Result b (Generator a) -> Generator (Result b a)
resultRandMap result =
    case result of
        Err err ->
            Random.constant (Err err)

        Ok rand ->
            rand |> Random.map Ok
