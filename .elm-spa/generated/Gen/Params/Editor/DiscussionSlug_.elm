module Gen.Params.Editor.DiscussionSlug_ exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    { discussionSlug : String }


parser =
    Parser.map Params (Parser.s "editor" </> Parser.string)
