module Bridge exposing (..)

import Data.Discussion.Filters exposing (Filters)
import Data.Game exposing (Rule)
import Data.Game.Player exposing (Player)
import Data.Game.Pointer exposing (Pointer)
import Data.User exposing (User)
import Lamdera


sendToBackend =
    Lamdera.sendToBackend


type ToBackend
    = SignedOut User
    | SpendToken (Pointer Rule)
      -- Req/resp paired messages
    | GetTags_Home_
    | DiscussionList_Home_ { filters : Filters, page : Int }
    | DiscussionFeed_Home_ { page : Int }
    | DiscussionList_Username_
    | DiscussionGet_Editor__DiscussionSlug_ { slug : String }
    | DiscussionGet_Discussion__Slug_ { slug : String }
    | DiscussionCreate_Editor
        { discussion :
            { title : String, description : String, body : String, tags : List String }
        }
    | DiscussionUpdate_Editor__DiscussionSlug_
        { slug : String
        , updates :
            { title : String, description : String, body : String, tags : List String }
        }
    | DiscussionDelete_Discussion__Slug_ { slug : String }
    | DiscussionFavorite_Profile__Id_ { slug : String }
    | DiscussionUnfavorite_Profile__Id_ { slug : String }
    | DiscussionFavorite_Home_ { slug : String }
    | DiscussionUnfavorite_Home_ { slug : String }
    | DiscussionFavorite_Discussion__Slug_ { slug : String }
    | DiscussionUnfavorite_Discussion__Slug_ { slug : String }
    | DiscussionCommentGet_Discussion__Slug_ { discussionSlug : String }
    | DiscussionCommentCreate_Discussion__Slug_ { discussionSlug : String, comment : { body : String } }
    | DiscussionCommentDelete_Discussion__Slug_ { discussionSlug : String, commentId : Int }
    | ProfileGet_Profile__Id_ { id : Int }
    | ProfileFollow_Profile__Id_ { id : Int }
    | ProfileUnfollow_Profile__Id_ { id : Int }
    | ProfileFollow_Discussion__Slug_ { id : Int }
    | ProfileUnfollow_Discussion__Slug_ { id : Int }
    | UserAuthentication_Login { params : { email : String, password : String } }
    | UserRegistration_Register { params : { username : String, email : String, password : String } }
    | UserUpdate_Settings
        { params :
            { username : String
            , email : String
            , oldPassword : Maybe String
            , newPassword : Maybe String
            , bio : String
            }
        }
    | NoOpToBackend
