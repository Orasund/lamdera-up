module Gen.Params.Discussion.Id_ exposing (Params, parser)

import Url.Parser as Parser exposing ((</>), Parser)


type alias Params =
    { id : String }


parser =
    Parser.map Params (Parser.s "discussion" </> Parser.string)
