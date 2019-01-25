module MatrixUtils exposing (cellNeighbours, detectFate, fieldMatrix, getAliveCells, isAlive, randomFieldMatrix)

import Array exposing (Array)
import Dict exposing (Dict)
import Matrix exposing (Matrix)
import Msgs exposing (Msg(..))
import Neighbours exposing (MatrixTopology(..), neighbours)
import String exposing (fromInt)
import Types exposing (Cell)


type alias Pattern =
    List (List Int)


sideSize : Int
sideSize =
    50


glider : Pattern
glider =
    [ [ 18, 22 ], [ 19, 22 ], [ 20, 22 ], [ 20, 21 ], [ 19, 20 ] ]


blinkers : Pattern
blinkers =
    [ [ 19, 20 ], [ 18, 21 ], [ 19, 21 ], [ 20, 21 ], [ 19, 22 ] ]


patterns : Dict String Pattern
patterns =
    Dict.fromList
        [ ( "glider", glider )
        , ( "blinkers", blinkers )
        ]


getPattern : String -> Pattern
getPattern patternName =
    case patterns |> Dict.get patternName of
        Just list ->
            list

        Nothing ->
            []


fieldMatrix : String -> Matrix Cell
fieldMatrix patternName =
    Matrix.generate sideSize
        sideSize
        (\x y ->
            let
                alive =
                    patternName |> getPattern |> isIncludedInPattern x y
            in
            Cell x y (cellId x y) alive
        )


randomFieldMatrix : Array Bool -> Matrix Cell
randomFieldMatrix seed =
    Matrix.generate sideSize
        sideSize
        (\x y -> Cell x y (cellId x y) False)
        |> Matrix.indexedMap
            (\x y cell ->
                let
                    seedIndex =
                        x + y * sideSize

                    alive =
                        case Array.get seedIndex seed of
                            Just value ->
                                value

                            Nothing ->
                                False
                in
                Cell x y (cellId x y) alive
            )


cellId x y =
    fromInt x ++ "-" ++ fromInt y


cellNeighbours : Matrix Cell -> Cell -> List Cell
cellNeighbours matrix { posX, posY } =
    neighbours Torus posX posY matrix |> Array.toList


liveNeighboursCount : Matrix Cell -> Cell -> Int
liveNeighboursCount matrix cell =
    cellNeighbours matrix cell
        |> List.filter .alive
        |> List.length


isIncludedInPattern : Int -> Int -> Pattern -> Bool
isIncludedInPattern itemX itemY pattern =
    pattern
        |> List.filter
            (\list ->
                case list of
                    [ x, y ] ->
                        itemX == x && itemY == y

                    _ ->
                        False
            )
        |> List.isEmpty
        |> not


isAlive : Matrix Cell -> Cell -> Bool
isAlive matrix cell =
    let
        count =
            liveNeighboursCount matrix cell
    in
    case cell.alive of
        True ->
            if count == 2 || count == 3 then
                True

            else
                False

        False ->
            count == 3


getAliveCells : Matrix Cell -> List Cell
getAliveCells matrix =
    matrix |> Matrix.toArray |> Array.filter .alive |> Array.toList


detectFate : Matrix Cell -> Cell -> Cell
detectFate matrix cell =
    { cell | alive = isAlive matrix cell }
