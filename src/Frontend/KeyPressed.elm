module Frontend.KeyPressed exposing (submitOnKeyDown)

import Browser.Events as Events
import Json.Decode as Decode exposing (Decoder)


submitOnKeyDown : { noOp : msg, submitted : msg } -> Sub msg
submitOnKeyDown args =
    Events.onKeyDown (keyDecoder args)


keyDecoder : { noOp : msg, submitted : msg } -> Decoder msg
keyDecoder args =
    Decode.map (toDirection args) (Decode.field "key" Decode.string)


toDirection : { noOp : msg, submitted : msg } -> String -> msg
toDirection args string =
    case string of
        "Enter" ->
            args.submitted

        _ ->
            args.noOp
