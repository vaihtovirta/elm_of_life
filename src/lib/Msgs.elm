module Msgs exposing (Msg(..))

import Time


type Msg
    = ClickCell String
    | RandomField (List Bool)
    | SelectPattern String
    | SelectSpeed String
    | Tick Time.Posix
    | ToggleTicking
