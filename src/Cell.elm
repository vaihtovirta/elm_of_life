module Cell exposing (..)

import Array exposing (fromList)
import Svg exposing (rect)
import Svg.Attributes exposing (fill, height, stroke, viewBox, width, x, y)
import Svg.Events exposing (onClick)
import Matrix exposing (Matrix)

cellSize =
    15

cell item onClickHanlder =
    let
        xString =
            String.fromInt (item.x * cellSize)

        yString =
            String.fromInt (item.y * cellSize)

        cellSizeString =
            String.fromInt cellSize

        fillColor =
            if item.alive then
                "green"

            else
                "white"
    in
    rect
        [ x xString
        , y yString
        , width cellSizeString
        , height cellSizeString
        , fill fillColor
        , stroke "black"
        , onClick onClickHanlder
        ]
        []

buildCellList generation onClickHanlder =
    generation
    |> Matrix.toArray
    |> Array.toList
    |> List.map(\item -> cell item (onClickHanlder item.id))
