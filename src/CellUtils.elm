module CellUtils exposing (cellList)

import Array exposing (fromList)
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
    if alive then
        "green"

    else
        "white"


cellRect : Cell -> (String -> msg) -> Svg msg
cellRect { posX, posY, alive, id } onClickHanlder =
    rect
        [ rectPosition posX |> x
        , rectPosition posY |> y
        , width cellSizeString
        , height cellSizeString
        , alive |> fillColor |> fill
        , stroke "black"
        , onClick (onClickHanlder id)
        ]
        []

cellList : Matrix Cell -> (String -> Msg) -> List(Svg Msg)
cellList generation onClickHanlder =
    generation
        |> Matrix.toArray
        |> Array.toList
        |> List.map (\item -> cellRect item onClickHanlder)
