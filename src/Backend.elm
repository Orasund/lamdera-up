module Backend exposing (..)

import Backend.Observer as Observer
import Bridge exposing (..)
import Config
import Data.Discussion exposing (Discussion, DiscussionStore)
import Data.Game as Game
import Data.Game.Player as Player
import Data.Response exposing (Response(..))
import Data.User as User exposing (UserFull)
import Dict
import Dict.Extra as Dict
import Gen.Msg
import Lamdera exposing (..)
import List.Extra as List
import Pages.Discussion.Slug_
import Pages.Editor
import Pages.Editor.DiscussionSlug_
import Pages.Home_
import Pages.Login
import Pages.Profile.Id_
import Pages.Register
import Pages.Settings
import Random
import Sha256
import Task
import Time
import Time.Extra as Time
import Types exposing (BackendModel, BackendMsg(..), FrontendMsg(..), ToFrontend(..))


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { sessions = Dict.empty
      , users = Dict.empty
      , discussions = Dict.empty
      , comments = Dict.empty
      , game = Game.init
      , daysPassed = 0
      , seed = Random.initialSeed 42
      }
        |> addPlayer { email = "bot1@bot.com", username = "Bot1", password = Sha256.sha256 "bot" }
        |> Tuple.first
        |> addPlayer { email = "bot2@bot.com", username = "Bot2", password = Sha256.sha256 "bot" }
        |> Tuple.first
        |> addPlayer { email = "bot3@bot.com", username = "Bot3", password = Sha256.sha256 "bot" }
        |> Tuple.first
    , Random.independentSeed
        |> Random.generate GotSeed
    )


addPlayer : { email : String, username : String, password : String } -> Model -> ( Model, UserFull )
addPlayer params model =
    let
        ( player, game ) =
            model.game
                |> Game.addPlayer Player.new

        user_ =
            User.new
                { id = Dict.size model.users
                , email = params.email
                , username = params.username
                , password = params.password
                , player = player
                }
    in
    ( { model
        | users = model.users |> Dict.insert user_.id user_
        , game = game
      }
    , user_
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        CheckSession sid cid ->
            model
                |> getSessionUser sid
                |> Maybe.map
                    (\user ->
                        ( model
                        , sendToFrontend cid (ActiveSession (User.toUser model.game user))
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        RenewSession uid sid cid now ->
            ( { model
                | sessions =
                    model.sessions
                        |> Dict.update sid (always (Just { userId = uid, expires = now |> Time.add Time.Day 30 Time.utc }))
              }
            , Time.now |> Task.perform (always (CheckSession sid cid))
            )

        DiscussionCreated t userM clientId discussion ->
            case userM of
                Just user ->
                    let
                        discussion_ =
                            { slug = uniqueSlug model discussion.title 1
                            , title = discussion.title
                            , description = ""
                            , createdAt = t
                            , updatedAt = t
                            , userId = user.id
                            }

                        res =
                            Success <| loadDiscussionFromStore model discussion_
                    in
                    ( { model
                        | discussions =
                            model.discussions
                                |> Dict.insert discussion_.slug discussion_
                      }
                    , [ Time.now
                            |> Task.perform (always (DiscussionCommentCreated t userM clientId discussion_.slug { body = discussion.description }))
                      , sendToFrontend clientId (PageMsg (Gen.Msg.Editor (Pages.Editor.GotDiscussion res)))
                      ]
                        |> Cmd.batch
                    )

                Nothing ->
                    ( model
                    , sendToFrontend clientId (PageMsg (Gen.Msg.Editor (Pages.Editor.GotDiscussion (Failure [ "invalid session" ]))))
                    )

        DiscussionCommentCreated t userM clientId slug commentBody ->
            case userM of
                Just user ->
                    let
                        comment =
                            { id = Time.posixToMillis t
                            , createdAt = t
                            , updatedAt = t
                            , body = commentBody.body
                            , author = User.toProfile model.game user
                            }

                        newComments =
                            model.comments
                                |> Dict.update slug
                                    (\commentsM ->
                                        case commentsM of
                                            Just comments ->
                                                Just (comments |> Dict.insert comment.id comment)

                                            Nothing ->
                                                Just <| Dict.singleton comment.id comment
                                    )
                    in
                    ( { model | comments = newComments }
                    , sendToFrontend clientId (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.CreatedComment (Success comment))))
                    )

                Nothing ->
                    ( model
                    , sendToFrontend clientId (PageMsg (Gen.Msg.Editor (Pages.Editor.GotDiscussion (Failure [ "invalid session" ]))))
                    )

        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        DayPassed ->
            let
                ( result, seed ) =
                    Random.step
                        (model.game
                            |> Game.triggerEvent (Game.DayPassed (model.daysPassed + 1))
                        )
                        model.seed
            in
            ( { model
                | seed = seed
                , daysPassed = model.daysPassed + 1
              }
                |> (\m ->
                        case result of
                            Ok g ->
                                { m | game = g }

                            Err _ ->
                                m
                   )
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        send v =
            ( model, send_ v )

        send_ v =
            sendToFrontend clientId v

        onlyWhenDiscussionOwner slug fn =
            onlyWhenDiscussionOwner_ slug (\r -> ( model, sendToFrontend clientId (fn r) ))

        onlyWhenDiscussionOwner_ slug fn =
            let
                res =
                    model |> loadDiscussionBySlug slug

                userM =
                    model |> getSessionUser sessionId
            in
            fn <|
                case ( res, userM ) of
                    ( Success discussion, Just user ) ->
                        if discussion.author.username == user.email then
                            res

                        else
                            Failure [ "you do not have permission for this discussion" ]

                    _ ->
                        Failure [ "you do not have permission for this discussion" ]
    in
    case msg of
        SignedOut _ ->
            ( { model | sessions = model.sessions |> Dict.remove sessionId }, Cmd.none )

        SpendToken { rule, amountSpent } ->
            model
                |> getSessionUser sessionId
                |> Maybe.map
                    (\userFull ->
                        let
                            ( result, seed ) =
                                Random.step
                                    (model.game
                                        |> Game.spendToken
                                            { rule = rule
                                            , current = userFull.player
                                            , amountSpent = amountSpent
                                            }
                                    )
                                    model.seed
                        in
                        { model
                            | seed = seed
                        }
                            |> (\m ->
                                    case result of
                                        Ok game ->
                                            ( { m | game = game }
                                            , game
                                                |> Observer.updatedGame { userFull = userFull, clientId = clientId }
                                            )

                                        Err err ->
                                            ( m
                                            , Cmd.none
                                            )
                               )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        DiscussionList_Discussion__Slug_ { page } ->
            let
                discussionList =
                    getListing model page
            in
            send (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotDiscussions (Success discussionList))))

        DiscussionList_Home_ { page } ->
            let
                discussionList =
                    getListing model page
            in
            send (PageMsg (Gen.Msg.Home_ (Pages.Home_.GotDiscussions (Success discussionList))))

        DiscussionFeed_Home_ { page } ->
            let
                userM =
                    model |> getSessionUser sessionId

                discussionList =
                    case userM of
                        Just user ->
                            let
                                filtered =
                                    model.discussions
                                        |> Dict.filter (\_ discussion -> List.member discussion.userId user.following)

                                enriched =
                                    filtered |> Dict.map (\_ -> loadDiscussionFromStore model)

                                grouped =
                                    enriched |> Dict.values |> List.greedyGroupsOf Data.Discussion.itemsPerPage

                                discussions =
                                    grouped |> List.getAt (page - 1) |> Maybe.withDefault []
                            in
                            { discussions = discussions
                            , page = page
                            , totalPages = grouped |> List.length
                            }

                        Nothing ->
                            { discussions = [], page = 0, totalPages = 0 }
            in
            send (PageMsg (Gen.Msg.Home_ (Pages.Home_.GotDiscussions (Success discussionList))))

        DiscussionList_Username_ ->
            model.game.rules
                |> Success
                |> Pages.Profile.Id_.GotRules
                |> Gen.Msg.Profile__Id_
                |> PageMsg
                |> send

        DiscussionGet_Editor__DiscussionSlug_ { slug } ->
            onlyWhenDiscussionOwner slug
                (\r -> PageMsg (Gen.Msg.Editor__DiscussionSlug_ (Pages.Editor.DiscussionSlug_.LoadedInitialDiscussion r)))

        DiscussionUpdate_Editor__DiscussionSlug_ { slug, updates } ->
            let
                discussions =
                    model.discussions
                        |> Dict.update slug
                            (Maybe.map
                                (\a ->
                                    { a
                                        | title = updates.title
                                        , description = updates.description
                                    }
                                )
                            )

                res =
                    discussions
                        |> Dict.get slug
                        |> Maybe.map Success
                        |> Maybe.withDefault (Failure [ "no discussion with slug: " ++ slug ])
                        |> Data.Response.map (loadDiscussionFromStore model)
            in
            ( { model | discussions = discussions }, send_ (PageMsg (Gen.Msg.Editor__DiscussionSlug_ (Pages.Editor.DiscussionSlug_.UpdatedDiscussion res))) )

        DiscussionGet_Discussion__Slug_ { slug } ->
            let
                res =
                    model |> loadDiscussionBySlug slug
            in
            send (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotDiscussion res)))

        DiscussionCreate_Editor { discussion } ->
            let
                userM =
                    model |> getSessionUser sessionId
            in
            ( model
            , Time.now
                |> Task.perform (\t -> DiscussionCreated t userM clientId discussion)
            )

        DiscussionDelete_Discussion__Slug_ { slug } ->
            onlyWhenDiscussionOwner_ slug
                (\r ->
                    ( { model | discussions = model.discussions |> Dict.remove slug }
                    , send_ (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.DeletedDiscussion r)))
                    )
                )

        DiscussionCommentGet_Discussion__Slug_ { discussionSlug } ->
            let
                res =
                    model.comments
                        |> Dict.get discussionSlug
                        |> Maybe.map Dict.values
                        |> Maybe.map (List.sortBy .id)
                        |> Maybe.map List.reverse
                        |> Maybe.map Success
                        |> Maybe.withDefault (Success [])
            in
            send (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotComments res)))

        DiscussionCommentCreate_Discussion__Slug_ { discussionSlug, comment } ->
            let
                userM =
                    model |> getSessionUser sessionId
            in
            ( model, Time.now |> Task.perform (\t -> DiscussionCommentCreated t userM clientId discussionSlug comment) )

        DiscussionCommentDelete_Discussion__Slug_ { discussionSlug, commentId } ->
            let
                newComments =
                    model.comments
                        |> Dict.update discussionSlug (Maybe.map (\comments -> Dict.remove commentId comments))
            in
            ( { model | comments = newComments }
            , send_ (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.DeletedComment (Success commentId))))
            )

        ProfileGet_Profile__Id_ { id } ->
            let
                res =
                    profileById id model
                        |> Maybe.map Success
                        |> Maybe.withDefault (Failure [ "user not found" ])
            in
            send (PageMsg (Gen.Msg.Profile__Id_ (Pages.Profile.Id_.GotProfile res)))

        UserAuthentication_Login { params } ->
            let
                ( response, cmd ) =
                    model.users
                        |> Dict.find (\_ u -> u.email == params.email)
                        |> Maybe.map
                            (\( _, u ) ->
                                if u.password == params.password then
                                    ( Success (User.toUser model.game u)
                                    , renewSession u.id sessionId clientId
                                    )

                                else
                                    ( Failure [ "email or password is invalid" ], Cmd.none )
                            )
                        |> Maybe.withDefault ( Failure [ "email or password is invalid" ], Cmd.none )
            in
            ( model, Cmd.batch [ send_ (PageMsg (Gen.Msg.Login (Pages.Login.GotUser response))), cmd ] )

        UserRegistration_Register { params } ->
            let
                ( model_, cmd, res ) =
                    if model.users |> Dict.any (\_ u -> u.email == params.email) then
                        ( model, Cmd.none, Failure [ "email address already taken" ] )

                    else
                        let
                            ( m, user_ ) =
                                model
                                    |> addPlayer params
                        in
                        ( m
                        , renewSession user_.id sessionId clientId
                        , Success (User.toUser model.game user_)
                        )
            in
            ( model_, Cmd.batch [ cmd, send_ (PageMsg (Gen.Msg.Register (Pages.Register.GotUser res))) ] )

        UserUpdate_Settings { params } ->
            let
                ( model_, res ) =
                    case model |> getSessionUser sessionId of
                        Just user ->
                            if
                                (params.oldPassword == Nothing)
                                    || (params.oldPassword == Just user.password)
                            then
                                let
                                    user_ =
                                        { user
                                            | username = params.username
                                            , email = params.email
                                            , password =
                                                params.newPassword
                                                    |> Maybe.withDefault user.password
                                            , bio = Just params.bio
                                        }
                                in
                                ( model |> updateUser user_
                                , Success (User.toUser model.game user_)
                                )

                            else
                                ( model, Failure [ "Old password is incorrect" ] )

                        Nothing ->
                            ( model, Failure [ "you do not have permission for this user" ] )
            in
            ( model_, send_ (PageMsg (Gen.Msg.Settings (Pages.Settings.GotUser res))) )

        NoOpToBackend ->
            ( model, Cmd.none )


getSessionUser : SessionId -> Model -> Maybe UserFull
getSessionUser sid model =
    model.sessions
        |> Dict.get sid
        |> Maybe.andThen (\session -> model.users |> Dict.get session.userId)


renewSession email sid cid =
    Time.now |> Task.perform (RenewSession email sid cid)


getListing : Model -> Int -> Data.Discussion.Listing
getListing model page =
    let
        enriched =
            model.discussions |> Dict.map (\_ -> loadDiscussionFromStore model)

        grouped =
            enriched |> Dict.values |> List.greedyGroupsOf Data.Discussion.itemsPerPage

        discussions =
            grouped |> List.getAt (page - 1) |> Maybe.withDefault []
    in
    { discussions = discussions
    , page = page
    , totalPages = grouped |> List.length
    }


loadDiscussionBySlug : String -> Model -> Response Discussion
loadDiscussionBySlug slug model =
    model.discussions
        |> Dict.get slug
        |> Maybe.map Success
        |> Maybe.withDefault (Failure [ "no discussion with slug: " ++ slug ])
        |> Data.Response.map (loadDiscussionFromStore model)


uniqueSlug : Model -> String -> Int -> String
uniqueSlug model title i =
    let
        slug =
            title |> String.replace " " "-"
    in
    if not (model.discussions |> Dict.member slug) then
        slug

    else if not (model.discussions |> Dict.member (slug ++ "-" ++ String.fromInt i)) then
        slug ++ "-" ++ String.fromInt i

    else
        uniqueSlug model title (i + 1)


updateUser : UserFull -> Model -> Model
updateUser user model =
    { model | users = model.users |> Dict.update user.id (Maybe.map (always user)) }


profileById id model =
    model.users
        |> Dict.find (\_ u -> u.id == id)
        |> Maybe.map (Tuple.second >> User.toProfile model.game)


profileByEmail email model =
    model.users
        |> Dict.find (\_ u -> u.email == email)
        |> Maybe.map (Tuple.second >> User.toProfile model.game)


loadDiscussionFromStore : Model -> DiscussionStore -> Discussion
loadDiscussionFromStore model store =
    let
        author =
            model.users
                |> Dict.get store.userId
                |> Maybe.map (User.toProfile model.game)
                |> Maybe.withDefault
                    { id = -1
                    , username = "error: unknown user"
                    , bio = Nothing
                    , following = False
                    , points = 0
                    }
    in
    { slug = store.slug
    , title = store.title
    , description = store.description
    , createdAt = store.createdAt
    , updatedAt = store.updatedAt
    , author = author
    }


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    let
        second =
            1000

        minute =
            60 * second

        hour =
            60 * minute

        day =
            24 * hour
    in
    Sub.batch
        [ onConnect CheckSession
        , Time.every
            (if Config.debugMode then
                minute

             else
                day
            )
            (always <| DayPassed)
        ]
