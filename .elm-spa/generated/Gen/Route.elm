module Gen.Route exposing
    ( Route(..)
    , fromUrl
    , toHref
    )

import Gen.Params.Editor
import Gen.Params.Home_
import Gen.Params.Login
import Gen.Params.NotFound
import Gen.Params.Register
import Gen.Params.Settings
import Gen.Params.Discussion.Slug_
import Gen.Params.Editor.DiscussionSlug_
import Gen.Params.Profile.Id_
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Editor
    | Home_
    | Login
    | NotFound
    | Register
    | Settings
    | Discussion__Slug_ { slug : String }
    | Editor__DiscussionSlug_ { discussionSlug : String }
    | Profile__Id_ { id : String }


fromUrl : Url -> Route
fromUrl =
    Parser.parse (Parser.oneOf routes) >> Maybe.withDefault NotFound


routes : List (Parser (Route -> a) a)
routes =
    [ Parser.map Home_ Gen.Params.Home_.parser
    , Parser.map Editor Gen.Params.Editor.parser
    , Parser.map Login Gen.Params.Login.parser
    , Parser.map NotFound Gen.Params.NotFound.parser
    , Parser.map Register Gen.Params.Register.parser
    , Parser.map Settings Gen.Params.Settings.parser
    , Parser.map Editor__DiscussionSlug_ Gen.Params.Editor.DiscussionSlug_.parser
    , Parser.map Profile__Id_ Gen.Params.Profile.Id_.parser
    , Parser.map Discussion__Slug_ Gen.Params.Discussion.Slug_.parser
    ]


toHref : Route -> String
toHref route =
    let
        joinAsHref : List String -> String
        joinAsHref segments =
            "/" ++ String.join "/" segments
    in
    case route of
        Editor ->
            joinAsHref [ "editor" ]
    
        Home_ ->
            joinAsHref []
    
        Login ->
            joinAsHref [ "login" ]
    
        NotFound ->
            joinAsHref [ "not-found" ]
    
        Register ->
            joinAsHref [ "register" ]
    
        Settings ->
            joinAsHref [ "settings" ]
    
        Discussion__Slug_ params ->
            joinAsHref [ "discussion", params.slug ]
    
        Editor__DiscussionSlug_ params ->
            joinAsHref [ "editor", params.discussionSlug ]
    
        Profile__Id_ params ->
            joinAsHref [ "profile", params.id ]

