module Gen.Msg exposing (Msg(..))

import Gen.Params.Discussion.Slug_
import Gen.Params.Editor
import Gen.Params.Editor.DiscussionSlug_
import Gen.Params.Home_
import Gen.Params.Login
import Gen.Params.NotFound
import Gen.Params.Profile.Id_
import Gen.Params.Register
import Gen.Params.Settings
import Pages.Discussion.Slug_
import Pages.Editor
import Pages.Editor.DiscussionSlug_
import Pages.Home_
import Pages.Login
import Pages.NotFound
import Pages.Profile.Id_
import Pages.Register
import Pages.Settings


type Msg
    = Editor Pages.Editor.Msg
    | Home_ Pages.Home_.Msg
    | Login Pages.Login.Msg
    | Register Pages.Register.Msg
    | Settings Pages.Settings.Msg
    | Discussion__Slug_ Pages.Discussion.Slug_.Msg
    | Editor__DiscussionSlug_ Pages.Editor.DiscussionSlug_.Msg
    | Profile__Id_ Pages.Profile.Id_.Msg
