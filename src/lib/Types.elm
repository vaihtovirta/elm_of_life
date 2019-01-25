module Types exposing (Cell)


type alias Cell =
    { posX : Int, posY : Int, id : String, alive : Bool }
