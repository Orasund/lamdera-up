module Backend exposing (..)

import Bridge exposing (..)
import Data.Discussion exposing (Discussion, DiscussionStore, Slug)
import Data.Discussion.Filters as Filters exposing (Filters(..))
import Data.Game as Game
import Data.Game.Player as Player
import Data.Profile exposing (Profile)
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
      }
    , Cmd.none
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
                            , description = discussion.description
                            , body = discussion.body
                            , tags = discussion.tags
                            , createdAt = t
                            , updatedAt = t
                            , userId = user.id
                            }

                        res =
                            Success <| loadDiscussionFromStore model userM discussion_
                    in
                    ( { model | discussions = model.discussions |> Dict.insert discussion_.slug discussion_ }
                    , sendToFrontend clientId (PageMsg (Gen.Msg.Editor (Pages.Editor.GotDiscussion res)))
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

        DayPassed ->
            ( { model
                | game =
                    model.game
                        |> Game.triggerEvent (Game.DayPassed (model.daysPassed + 1))
                , daysPassed = model.daysPassed + 1
              }
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
                    model |> loadDiscussionBySlug slug sessionId

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

        SpendToken { rule, player } ->
            ( { model
                | game =
                    model.game
                        |> Game.spendToken
                            { rule = rule
                            , current = player
                            }
              }
            , Cmd.none
            )

        GetTags_Home_ ->
            let
                allTags =
                    model.discussions |> Dict.foldl (\slug discussion tags -> tags ++ discussion.tags) [] |> List.unique
            in
            send (PageMsg (Gen.Msg.Home_ (Pages.Home_.GotTags (Success allTags))))

        DiscussionList_Home_ { filters, page } ->
            let
                discussionList =
                    getListing model sessionId filters page
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
                                        |> Dict.filter (\slug discussion -> List.member discussion.userId user.following)

                                enriched =
                                    filtered |> Dict.map (\slug discussion -> loadDiscussionFromStore model userM discussion)

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
                                (\a -> { a | title = updates.title, body = updates.body, tags = updates.tags })
                            )

                res =
                    discussions
                        |> Dict.get slug
                        |> Maybe.map Success
                        |> Maybe.withDefault (Failure [ "no discussion with slug: " ++ slug ])
                        |> Data.Response.map (loadDiscussionFromStore model (model |> getSessionUser sessionId))
            in
            ( { model | discussions = discussions }, send_ (PageMsg (Gen.Msg.Editor__DiscussionSlug_ (Pages.Editor.DiscussionSlug_.UpdatedDiscussion res))) )

        DiscussionGet_Discussion__Slug_ { slug } ->
            let
                res =
                    model |> loadDiscussionBySlug slug sessionId
            in
            send (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotDiscussion res)))

        DiscussionCreate_Editor { discussion } ->
            let
                userM =
                    model |> getSessionUser sessionId
            in
            ( model, Time.now |> Task.perform (\t -> DiscussionCreated t userM clientId discussion) )

        DiscussionDelete_Discussion__Slug_ { slug } ->
            onlyWhenDiscussionOwner_ slug
                (\r ->
                    ( { model | discussions = model.discussions |> Dict.remove slug }
                    , send_ (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.DeletedDiscussion r)))
                    )
                )

        DiscussionFavorite_Profile__Id_ { slug } ->
            ( model, Cmd.none )

        DiscussionUnfavorite_Profile__Id_ { slug } ->
            ( model, Cmd.none )

        DiscussionFavorite_Home_ { slug } ->
            favoriteDiscussion sessionId
                slug
                model
                (\r -> send_ (PageMsg (Gen.Msg.Home_ (Pages.Home_.UpdatedDiscussion r))))

        DiscussionUnfavorite_Home_ { slug } ->
            unfavoriteDiscussion sessionId
                slug
                model
                (\r -> send_ (PageMsg (Gen.Msg.Home_ (Pages.Home_.UpdatedDiscussion r))))

        DiscussionFavorite_Discussion__Slug_ { slug } ->
            favoriteDiscussion sessionId
                slug
                model
                (\r -> send_ (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotDiscussion r))))

        DiscussionUnfavorite_Discussion__Slug_ { slug } ->
            unfavoriteDiscussion sessionId
                slug
                model
                (\r -> send_ (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotDiscussion r))))

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

        ProfileFollow_Profile__Id_ { id } ->
            followUser sessionId
                id
                model
                (\r -> send_ (PageMsg (Gen.Msg.Profile__Id_ (Pages.Profile.Id_.GotProfile r))))

        ProfileUnfollow_Profile__Id_ { id } ->
            unfollowUser sessionId
                id
                model
                (\r -> send_ (PageMsg (Gen.Msg.Profile__Id_ (Pages.Profile.Id_.GotProfile r))))

        ProfileFollow_Discussion__Slug_ { id } ->
            followUser sessionId
                id
                model
                (\r -> send_ (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotAuthor r))))

        ProfileUnfollow_Discussion__Slug_ { id } ->
            unfollowUser sessionId
                id
                model
                (\r -> send_ (PageMsg (Gen.Msg.Discussion__Slug_ (Pages.Discussion.Slug_.GotAuthor r))))

        UserAuthentication_Login { params } ->
            let
                ( response, cmd ) =
                    model.users
                        |> Dict.find (\k u -> u.email == params.email)
                        |> Maybe.map
                            (\( k, u ) ->
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
                    if model.users |> Dict.any (\k u -> u.email == params.email) then
                        ( model, Cmd.none, Failure [ "email address already taken" ] )

                    else
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
                                ( model |> updateUser user_, Success (User.toUser model.game user_) )

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


getListing : Model -> SessionId -> Filters -> Int -> Data.Discussion.Listing
getListing model sessionId (Filters { tag, author, favorited }) page =
    let
        filtered =
            model.discussions
                |> Filters.byFavorite favorited model.users
                |> Filters.byTag tag
                |> Filters.byAuthor author model.users

        enriched =
            filtered |> Dict.map (\slug discussion -> loadDiscussionFromStore model (model |> getSessionUser sessionId) discussion)

        grouped =
            enriched |> Dict.values |> List.greedyGroupsOf Data.Discussion.itemsPerPage

        discussions =
            grouped |> List.getAt (page - 1) |> Maybe.withDefault []
    in
    { discussions = discussions
    , page = page
    , totalPages = grouped |> List.length
    }


loadDiscussionBySlug : String -> SessionId -> Model -> Response Discussion
loadDiscussionBySlug slug sid model =
    model.discussions
        |> Dict.get slug
        |> Maybe.map Success
        |> Maybe.withDefault (Failure [ "no discussion with slug: " ++ slug ])
        |> Data.Response.map (loadDiscussionFromStore model (model |> getSessionUser sid))


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


favoriteDiscussion : SessionId -> Slug -> Model -> (Response Discussion -> Cmd msg) -> ( Model, Cmd msg )
favoriteDiscussion sessionId slug model toResponseCmd =
    let
        res =
            model
                |> loadDiscussionBySlug slug sessionId
                |> Data.Response.map (\a -> { a | favorited = True })
    in
    case model |> getSessionUser sessionId of
        Just user ->
            ( if model.discussions |> Dict.member slug then
                model |> updateUser { user | favorites = (slug :: user.favorites) |> List.unique }

              else
                model
            , toResponseCmd res
            )

        Nothing ->
            ( model, toResponseCmd <| Failure [ "invalid session" ] )


unfavoriteDiscussion : SessionId -> Slug -> Model -> (Response Discussion -> Cmd msg) -> ( Model, Cmd msg )
unfavoriteDiscussion sessionId slug model toResponseCmd =
    let
        res =
            model
                |> loadDiscussionBySlug slug sessionId
                |> Data.Response.map (\a -> { a | favorited = False })
    in
    case model |> getSessionUser sessionId of
        Just user ->
            ( model |> updateUser { user | favorites = user.favorites |> List.remove slug }
            , toResponseCmd res
            )

        Nothing ->
            ( model, toResponseCmd <| Failure [ "invalid session" ] )


followUser : SessionId -> Int -> Model -> (Response Profile -> Cmd msg) -> ( Model, Cmd msg )
followUser sessionId id model toResponseCmd =
    let
        res =
            profileById id model
                |> Maybe.map (\a -> Success { a | following = True })
                |> Maybe.withDefault (Failure [ "invalid user" ])
    in
    case model |> getSessionUser sessionId of
        Just user ->
            ( case model.users |> Dict.find (\l u -> u.id == id) of
                Just ( _, follow ) ->
                    model |> updateUser { user | following = (follow.id :: user.following) |> List.unique }

                Nothing ->
                    model
            , toResponseCmd res
            )

        Nothing ->
            ( model, toResponseCmd <| Failure [ "invalid session" ] )


unfollowUser : SessionId -> Int -> Model -> (Response Profile -> Cmd msg) -> ( Model, Cmd msg )
unfollowUser sessionId id model toResponseCmd =
    case model.users |> Dict.find (\k u -> u.id == id) of
        Just ( _, followed ) ->
            let
                res =
                    followed
                        |> User.toProfile model.game
                        |> (\a -> Success { a | following = False })
            in
            case model |> getSessionUser sessionId of
                Just user ->
                    ( model
                        |> updateUser
                            { user
                                | following = user.following |> List.remove followed.id
                            }
                    , toResponseCmd res
                    )

                Nothing ->
                    ( model, toResponseCmd <| Failure [ "invalid session" ] )

        Nothing ->
            ( model, toResponseCmd <| Failure [ "invalid user" ] )


updateUser : UserFull -> Model -> Model
updateUser user model =
    { model | users = model.users |> Dict.update user.id (Maybe.map (always user)) }


profileById id model =
    model.users
        |> Dict.find (\k u -> u.id == id)
        |> Maybe.map (Tuple.second >> User.toProfile model.game)


profileByEmail email model =
    model.users
        |> Dict.find (\k u -> u.email == email)
        |> Maybe.map (Tuple.second >> User.toProfile model.game)


loadDiscussionFromStore : Model -> Maybe UserFull -> DiscussionStore -> Discussion
loadDiscussionFromStore model userM store =
    let
        favorited =
            userM |> Maybe.map (\user -> user.favorites |> List.member store.slug) |> Maybe.withDefault False

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
    , body = store.body
    , tags = store.tags
    , createdAt = store.createdAt
    , updatedAt = store.updatedAt
    , favorited = favorited
    , favoritesCount = model.users |> Dict.filter (\_ user -> user.favorites |> List.member store.slug) |> Dict.size
    , author = author
    }


subscriptions : Model -> Sub BackendMsg
subscriptions m =
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
        , Time.every day (always <| DayPassed)
        ]
