module MatrixUtils exposing (fieldMatrix, isAlive)

import Array exposing (Array)
import Matrix exposing (Matrix)
import Neighbours exposing (MatrixTopology(..), neighbours)
import ItemModel exposing(Item)
import String exposing (fromInt)

glider =
    [ [ 18, 22 ], [ 19, 22 ], [ 20, 22 ], [ 20, 21 ], [ 19, 20 ] ]

fieldMatrix : Matrix Item
fieldMatrix =
    Matrix.generate 50 50
        (\x y ->
            let
                id =
                    fromInt x ++ "-" ++ fromInt y

                alive =
                    isPrepopulated x y glider
            in
            Item x y id alive
        )


itemNeighbours : Matrix Item -> Item -> Array.Array Item
itemNeighbours matrix item =
    neighbours Plane item.x item.y matrix


liveNeighboursCount : Matrix Item -> Item -> Int
liveNeighboursCount matrix item =
    itemNeighbours matrix item
        |> Array.toList
        |> List.filter (\{ alive } -> alive)
        |> List.length


isPrepopulated itemX itemY figure =
    figure
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


isAlive : Matrix Item -> Item -> Bool
isAlive matrix item =
    let
        count =
            liveNeighboursCount matrix item
    in
    case item.alive of
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
