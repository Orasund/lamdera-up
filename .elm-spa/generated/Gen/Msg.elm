module Gen.Msg exposing (Msg(..))

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


type Msg
    = Editor Pages.Editor.Msg
    | Game Pages.Game.Msg
    | Home_ Pages.Home_.Msg
    | Login Pages.Login.Msg
    | NotFound Pages.NotFound.Msg
    | Register Pages.Register.Msg
    | Settings Pages.Settings.Msg
    | Discussion__Slug_ Pages.Discussion.Slug_.Msg
    | Editor__DiscussionSlug_ Pages.Editor.DiscussionSlug_.Msg
    | Profile__Id_ Pages.Profile.Id_.Msg

