module Msgs exposing (Msg(..))

import Time


type Msg
    = ClickEvent String
    | Tick Time.Posix
    | ToggleTicking
    | ChangePattern String
    | RandomField (List Bool)
