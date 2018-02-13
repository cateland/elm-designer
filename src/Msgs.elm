module Msgs exposing (Msg(..))

import Mouse exposing (Position)


type Msg
    = Press Position
    | Release Position
    | Move Position
    | Zoom Int
