module Types exposing (..)

import Bridge
import Browser
import Browser.Navigation exposing (Key)
import Data.Discussion exposing (DiscussionStore, Slug)
import Data.Discussion.Comment exposing (Comment)
import Data.Game exposing (Game)
import Data.User exposing (User, UserFull, UserId)
import Dict exposing (Dict)
import Gen.Pages as Pages
import Lamdera exposing (ClientId, SessionId)
import Random exposing (Seed)
import Shared
import Time
import Url exposing (Url)


type alias FrontendModel =
    { url : Url
    , key : Key
    , shared : Shared.Model
    , page : Pages.Model
    }


type alias BackendModel =
    { sessions : Dict SessionId Session
    , users : Dict Int UserFull
    , discussions : Dict Slug DiscussionStore
    , comments : Dict Slug (Dict Int Comment)
    , game : Game
    , daysPassed : Int
    , seed : Seed
    }


type alias Session =
    { userId : Int, expires : Time.Posix }


type FrontendMsg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | Shared Shared.Msg
    | Page Pages.Msg
    | Noop


type alias ToBackend =
    Bridge.ToBackend


type BackendMsg
    = CheckSession SessionId ClientId
    | RenewSession UserId SessionId ClientId Time.Posix
    | DiscussionCreated Time.Posix (Maybe UserFull) ClientId { title : String, description : String, body : String, tags : List String }
    | DiscussionCommentCreated Time.Posix (Maybe UserFull) ClientId Slug { body : String }
    | GotSeed Seed
    | DayPassed
    | NoOpBackendMsg


type ToFrontend
    = ActiveSession User
    | PageMsg Pages.Msg
    | NoOpToFrontend
