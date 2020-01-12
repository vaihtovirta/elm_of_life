module CellUtils exposing (cellList)

import Array exposing (Array)
import Matrix exposing (Matrix)
import Msgs exposing (Msg(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Types exposing (Cell)


cellSize : Int
cellSize =
    15


cellSizeString : String
cellSizeString =
    "15px"


rectPosition : Int -> String
rectPosition value =
    String.fromInt (value * cellSize) ++ "px"


fillColor : Bool -> String
fillColor alive =
    case alive of
        True ->
            "#5c8d89"

        False ->
            "#f9f8eb"


cellRect : Cell -> (String -> msg) -> Html msg
cellRect { posX, posY, alive, id } onClickHanlder =
    div
    [ style "background-color" (fillColor alive)
    , style "border" "1px solid gainsboro"
    , style "height" cellSizeString
    , style "width" cellSizeString
    , style "position" "absolute"
    , style "top" (rectPosition posY)
    , style "left" (rectPosition posX)
    , onClick (onClickHanlder id)
    ]
    []


cellList : Matrix Cell -> (String -> Msg) -> List (Html Msg)
cellList generation onClickHanlder =
    generation
        |> Matrix.toArray
        |> Array.map (\item -> cellRect item onClickHanlder)
        |> Array.toList
