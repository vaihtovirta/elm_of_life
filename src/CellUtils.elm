module CellUtils exposing (cellList)

import Array exposing (Array)
import Matrix exposing (Matrix)
import Svg exposing (Svg, rect)
import Svg.Attributes exposing (fill, height, stroke, width, x, y)
import Svg.Events exposing (onClick)
import Types exposing (Cell)
import Msgs exposing (Msg(..))


cellSize : Int
cellSize =
    15

cellSizeString : String
cellSizeString =
    "15"


rectPosition : Int -> String
rectPosition value =
    String.fromInt (value * cellSize)


fillColor : Bool -> String
fillColor alive =
    case alive of
        True ->
            "#5c8d89"

        False ->
            "#f9f8eb"


cellRect : Cell -> (String -> msg) -> Svg msg
cellRect { posX, posY, alive, id } onClickHanlder =
    rect
        [ posX |> rectPosition  |> x
        , posY |> rectPosition  |> y
        , width cellSizeString
        , height cellSizeString
        , alive |> fillColor |> fill
        , stroke "gainsboro"
        , onClick (onClickHanlder id)
        ]
        []

cellList : Matrix Cell -> (String -> Msg) -> List(Svg Msg)
cellList generation onClickHanlder =
    generation
        |> Matrix.toArray
        |> Array.map (\item -> cellRect item onClickHanlder)
        |> Array.toList
