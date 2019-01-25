module Msgs exposing (Msg(..))

import Time


type Msg
    = ClickCell String
    | Tick Time.Posix
    | ToggleTicking
    | SelectPattern String
    | RandomField (List Bool)
