module MatrixUtils exposing (fieldMatrix, randomFieldMatrix, isAlive)

import Array exposing (Array)
import Dict exposing (Dict)
import Matrix exposing (Matrix)
import Neighbours exposing (MatrixTopology(..), neighbours)
import Types exposing(Cell)
import String exposing (fromInt)
import Msgs exposing (Msg(..))

type alias Pattern = List(List Int)

glider : Pattern
glider =
    [ [ 18, 22 ], [ 19, 22 ], [ 20, 22 ], [ 20, 21 ], [ 19, 20 ] ]

blinkers : Pattern
blinkers =
    [ [ 19, 20 ], [18, 21], [ 19, 21 ], [20, 21], [19, 22] ]

patterns : Dict String Pattern
patterns =
  Dict.fromList
    [ ("glider", glider)
    , ("blinkers", blinkers)
    ]


getPattern : String -> Pattern
getPattern patternName =
    case patterns |> Dict.get patternName of
        Just list -> list
        Nothing -> []


fieldMatrix : String -> Matrix Cell
fieldMatrix patternName =
    Matrix.generate 50 50
        (\x y ->
            let
                alive =
                    patternName |> getPattern |> isIncludedInPattern x y
            in
            Cell x y (cellId x y) alive
        )


randomFieldMatrix : Array Bool -> Matrix Cell
randomFieldMatrix seed =
    Matrix.generate 50 50
        (\x y -> Cell x y (cellId x y) False)
    |> Matrix.indexedMap(\x y cell ->
        let
            seedIndex = x + y * 50
            alive = case Array.get(seedIndex) seed of
                Just value -> value
                Nothing -> False
        in
        Cell x y (cellId x y) alive
    )


cellId x y =
    fromInt x ++ "-" ++ fromInt y


cellNeighbours : Matrix Cell -> Cell -> Array.Array Cell
cellNeighbours matrix { posX, posY } =
    neighbours Torus posX posY matrix


liveNeighboursCount : Matrix Cell -> Cell -> Int
liveNeighboursCount matrix cell =
    cellNeighbours matrix cell
        |> Array.toList
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
            case count of
                0 ->
                    False

                1 ->
                    False

                2 ->
                    True

                3 ->
                    True

                _ ->
                    False

        False ->
            case count of
                3 ->
                    True

                _ ->
                    False
