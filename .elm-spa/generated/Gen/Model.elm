module Gen.Model exposing (Model(..))

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


type Model
    = Redirecting_
    | Editor Gen.Params.Editor.Params Pages.Editor.Model
    | Game Gen.Params.Game.Params Pages.Game.Model
    | Home_ Gen.Params.Home_.Params Pages.Home_.Model
    | Login Gen.Params.Login.Params Pages.Login.Model
    | NotFound Gen.Params.NotFound.Params Pages.NotFound.Model
    | Register Gen.Params.Register.Params Pages.Register.Model
    | Settings Gen.Params.Settings.Params Pages.Settings.Model
    | Discussion__Slug_ Gen.Params.Discussion.Slug_.Params Pages.Discussion.Slug_.Model
    | Editor__DiscussionSlug_ Gen.Params.Editor.DiscussionSlug_.Params Pages.Editor.DiscussionSlug_.Model
    | Profile__Id_ Gen.Params.Profile.Id_.Params Pages.Profile.Id_.Model

