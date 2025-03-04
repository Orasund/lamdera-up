module Gen.Pages exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation exposing (Key)
import Effect exposing (Effect)
import ElmSpa.Page
import Gen.Params.Editor
import Gen.Params.Game
import Gen.Params.Home_
import Gen.Params.Login
import Gen.Params.NotFound
import Gen.Params.Register
import Gen.Params.Settings
import Gen.Params.Discussion.Slug_
import Gen.Params.Editor.DiscussionSlug_
import Gen.Params.Profile.Id_
import Gen.Model as Model
import Gen.Msg as Msg
import Gen.Route as Route exposing (Route)
import Page exposing (Page)
import Pages.Editor
import Pages.Game
import Pages.Home_
import Pages.Login
import Pages.NotFound
import Pages.Register
import Pages.Settings
import Pages.Discussion.Slug_
import Pages.Editor.DiscussionSlug_
import Pages.Profile.Id_
import Request exposing (Request)
import Shared
import Task
import Url exposing (Url)
import View exposing (View)


type alias Model =
    Model.Model


type alias Msg =
    Msg.Msg


init : Route -> Shared.Model -> Url -> Key -> ( Model, Effect Msg )
init route =
    case route of
        Route.Editor ->
            pages.editor.init ()
    
        Route.Game ->
            pages.game.init ()
    
        Route.Home_ ->
            pages.home_.init ()
    
        Route.Login ->
            pages.login.init ()
    
        Route.NotFound ->
            pages.notFound.init ()
    
        Route.Register ->
            pages.register.init ()
    
        Route.Settings ->
            pages.settings.init ()
    
        Route.Discussion__Slug_ params ->
            pages.discussion__slug_.init params
    
        Route.Editor__DiscussionSlug_ params ->
            pages.editor__discussionSlug_.init params
    
        Route.Profile__Id_ params ->
            pages.profile__id_.init params


update : Msg -> Model -> Shared.Model -> Url -> Key -> ( Model, Effect Msg )
update msg_ model_ =
    case ( msg_, model_ ) of
        ( Msg.Editor msg, Model.Editor params model ) ->
            pages.editor.update params msg model
    
        ( Msg.Game msg, Model.Game params model ) ->
            pages.game.update params msg model
    
        ( Msg.Home_ msg, Model.Home_ params model ) ->
            pages.home_.update params msg model
    
        ( Msg.Login msg, Model.Login params model ) ->
            pages.login.update params msg model
    
        ( Msg.NotFound msg, Model.NotFound params model ) ->
            pages.notFound.update params msg model
    
        ( Msg.Register msg, Model.Register params model ) ->
            pages.register.update params msg model
    
        ( Msg.Settings msg, Model.Settings params model ) ->
            pages.settings.update params msg model
    
        ( Msg.Discussion__Slug_ msg, Model.Discussion__Slug_ params model ) ->
            pages.discussion__slug_.update params msg model
    
        ( Msg.Editor__DiscussionSlug_ msg, Model.Editor__DiscussionSlug_ params model ) ->
            pages.editor__discussionSlug_.update params msg model
    
        ( Msg.Profile__Id_ msg, Model.Profile__Id_ params model ) ->
            pages.profile__id_.update params msg model

        _ ->
            \_ _ _ -> ( model_, Effect.none )


view : Model -> Shared.Model -> Url -> Key -> View Msg
view model_ =
    case model_ of
        Model.Redirecting_ ->
            \_ _ _ -> View.none
    
        Model.Editor params model ->
            pages.editor.view params model
    
        Model.Game params model ->
            pages.game.view params model
    
        Model.Home_ params model ->
            pages.home_.view params model
    
        Model.Login params model ->
            pages.login.view params model
    
        Model.NotFound params model ->
            pages.notFound.view params model
    
        Model.Register params model ->
            pages.register.view params model
    
        Model.Settings params model ->
            pages.settings.view params model
    
        Model.Discussion__Slug_ params model ->
            pages.discussion__slug_.view params model
    
        Model.Editor__DiscussionSlug_ params model ->
            pages.editor__discussionSlug_.view params model
    
        Model.Profile__Id_ params model ->
            pages.profile__id_.view params model


subscriptions : Model -> Shared.Model -> Url -> Key -> Sub Msg
subscriptions model_ =
    case model_ of
        Model.Redirecting_ ->
            \_ _ _ -> Sub.none
    
        Model.Editor params model ->
            pages.editor.subscriptions params model
    
        Model.Game params model ->
            pages.game.subscriptions params model
    
        Model.Home_ params model ->
            pages.home_.subscriptions params model
    
        Model.Login params model ->
            pages.login.subscriptions params model
    
        Model.NotFound params model ->
            pages.notFound.subscriptions params model
    
        Model.Register params model ->
            pages.register.subscriptions params model
    
        Model.Settings params model ->
            pages.settings.subscriptions params model
    
        Model.Discussion__Slug_ params model ->
            pages.discussion__slug_.subscriptions params model
    
        Model.Editor__DiscussionSlug_ params model ->
            pages.editor__discussionSlug_.subscriptions params model
    
        Model.Profile__Id_ params model ->
            pages.profile__id_.subscriptions params model



-- INTERNALS


pages :
    { editor : Bundle Gen.Params.Editor.Params Pages.Editor.Model Pages.Editor.Msg
    , game : Bundle Gen.Params.Game.Params Pages.Game.Model Pages.Game.Msg
    , home_ : Bundle Gen.Params.Home_.Params Pages.Home_.Model Pages.Home_.Msg
    , login : Bundle Gen.Params.Login.Params Pages.Login.Model Pages.Login.Msg
    , notFound : Bundle Gen.Params.NotFound.Params Pages.NotFound.Model Pages.NotFound.Msg
    , register : Bundle Gen.Params.Register.Params Pages.Register.Model Pages.Register.Msg
    , settings : Bundle Gen.Params.Settings.Params Pages.Settings.Model Pages.Settings.Msg
    , discussion__slug_ : Bundle Gen.Params.Discussion.Slug_.Params Pages.Discussion.Slug_.Model Pages.Discussion.Slug_.Msg
    , editor__discussionSlug_ : Bundle Gen.Params.Editor.DiscussionSlug_.Params Pages.Editor.DiscussionSlug_.Model Pages.Editor.DiscussionSlug_.Msg
    , profile__id_ : Bundle Gen.Params.Profile.Id_.Params Pages.Profile.Id_.Model Pages.Profile.Id_.Msg
    }
pages =
    { editor = bundle Pages.Editor.page Model.Editor Msg.Editor
    , game = bundle Pages.Game.page Model.Game Msg.Game
    , home_ = bundle Pages.Home_.page Model.Home_ Msg.Home_
    , login = bundle Pages.Login.page Model.Login Msg.Login
    , notFound = bundle Pages.NotFound.page Model.NotFound Msg.NotFound
    , register = bundle Pages.Register.page Model.Register Msg.Register
    , settings = bundle Pages.Settings.page Model.Settings Msg.Settings
    , discussion__slug_ = bundle Pages.Discussion.Slug_.page Model.Discussion__Slug_ Msg.Discussion__Slug_
    , editor__discussionSlug_ = bundle Pages.Editor.DiscussionSlug_.page Model.Editor__DiscussionSlug_ Msg.Editor__DiscussionSlug_
    , profile__id_ = bundle Pages.Profile.Id_.page Model.Profile__Id_ Msg.Profile__Id_
    }


type alias Bundle params model msg =
    ElmSpa.Page.Bundle params model msg Shared.Model (Effect Msg) Model Msg (View Msg)


bundle page toModel toMsg =
    ElmSpa.Page.bundle
        { redirecting =
            { model = Model.Redirecting_
            , view = View.none
            }
        , toRoute = Route.fromUrl
        , toUrl = Route.toHref
        , fromCmd = Effect.fromCmd
        , mapEffect = Effect.map toMsg
        , mapView = View.map toMsg
        , toModel = toModel
        , toMsg = toMsg
        , page = page
        }


type alias Static params =
    Bundle params () Never


static : View Never -> (params -> Model) -> Static params
static view_ toModel =
    { init = \params _ _ _ -> ( toModel params, Effect.none )
    , update = \params _ _ _ _ _ -> ( toModel params, Effect.none )
    , view = \_ _ _ _ _ -> View.map never view_
    , subscriptions = \_ _ _ _ _ -> Sub.none
    }
    
