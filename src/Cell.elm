module Cell exposing (buildCellList)

import Array exposing (fromList)
import Matrix exposing (Matrix)
import Svg exposing (rect)
import Svg.Attributes exposing (fill, height, stroke, viewBox, width, x, y)
import Svg.Events exposing (onClick)


cellSize =
    15


cellSizeString =
    "15"


rectPosition value =
    String.fromInt (value * cellSize)


fillColor alive =
    if alive then
        "green"

    else
        "white"


cell item onClickHanlder =
    rect
        [ rectPosition item.x |> x
        , rectPosition item.y |> y
        , width cellSizeString
        , height cellSizeString
        , item.alive |> fillColor |> fill
        , stroke "black"
        , onClick onClickHanlder
        ]
        []


buildCellList generation onClickHanlder =
    generation
        |> Matrix.toArray
        |> Array.toList
        |> List.map (\item -> cell item (onClickHanlder item.id))
