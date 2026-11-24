-- Copyright (c) 2026 Derek Gustafson.


port module Ports exposing (..)

import Json.Encode as Encode


port saveState : Encode.Value -> Cmd msg


port log : String -> Cmd msg


port exportState : Encode.Value -> Cmd msg


port openFile : () -> Cmd msg


port fileLoaded : (String -> msg) -> Sub msg
