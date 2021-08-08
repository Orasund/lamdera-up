module Bridge exposing (..)

import Data.Game exposing (Rule)
import Data.Game.Pointer exposing (Pointer)
import Data.User exposing (User)
import Lamdera


sendToBackend =
    Lamdera.sendToBackend


type ToBackend
    = SignedOut User
    | SpendToken (Pointer Rule)
      -- Req/resp paired messages
    | DiscussionList_Home_ { page : Int }
    | DiscussionFeed_Home_ { page : Int }
    | DiscussionList_Username_
    | DiscussionGet_Editor__DiscussionSlug_ { slug : String }
    | DiscussionGet_Discussion__Slug_ { slug : String }
    | DiscussionCreate_Editor
        { discussion :
            { title : String, description : String }
        }
    | DiscussionUpdate_Editor__DiscussionSlug_
        { slug : String
        , updates :
            { title : String, description : String }
        }
    | DiscussionDelete_Discussion__Slug_ { slug : String }
    | DiscussionCommentGet_Discussion__Slug_ { discussionSlug : String }
    | DiscussionCommentCreate_Discussion__Slug_ { discussionSlug : String, comment : { body : String } }
    | DiscussionCommentDelete_Discussion__Slug_ { discussionSlug : String, commentId : Int }
    | DiscussionList_Discussion__Slug_ { page : Int }
    | ProfileGet_Profile__Id_ { id : Int }
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
