module Main exposing (Model)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (uniqueBy)
import Matrix exposing (Matrix)
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Time

import CellUtils exposing (cellList)
import MatrixUtils exposing (cellNeighbours, fieldMatrix, isAlive, getAliveCells, randomFieldMatrix, detectFate)
import Msgs exposing (Msg(..))
import Types exposing (Cell)


viewBoxSize : String
viewBoxSize =
    "800"


viewBoxParams : String
viewBoxParams =
    "0 0 800 800"


tickRate : Float
tickRate =
    100


type alias Model =
    { generation : Matrix Cell
    , selectedPattern : String
    , isTicking : Bool
    , aliveCells : List Cell
    }


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        glider = fieldMatrix "glider"
    in
    ( { generation = fieldMatrix "glider"
      , selectedPattern = "glider"
      , isTicking = False
      , aliveCells = getAliveCells glider
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                live = fetchLiveCells model
                (newGeneration, newLiveCells) = buildNewGeneration model.generation live
                isTicking = List.length live > 0
            in
            ( { model | generation = newGeneration
            , aliveCells = newLiveCells
            , isTicking = isTicking
            }
            , Cmd.none
            )

        ClickEvent id ->
            ( { model | generation = Matrix.map (toggle id) model.generation }
            , Cmd.none
            )

        ToggleTicking ->
            ( { model | isTicking = not model.isTicking }
            , Cmd.none
            )

        ChangePattern "random" ->
            ( model
            , Random.weighted ( 20, True ) [ ( 80, False ) ]
                |> Random.list 2500
                |> Random.generate RandomField
            )

        ChangePattern patternName ->
            ( { model
                | isTicking = False
                , selectedPattern = patternName
                , aliveCells = []
                , generation = fieldMatrix patternName
              }
            , Cmd.none
            )

        RandomField seed ->
            ( { model
                | isTicking = False
                , selectedPattern = "random"
                , aliveCells = []
                , generation = seed |> Array.fromList |> randomFieldMatrix
              }
            , Cmd.none
            )


view : Model -> Html Msg
view { generation, isTicking, selectedPattern } =
    div []
        [ button
            [ style "margin" "10px"
            , onClick ToggleTicking
            ]
            [ isTicking |> buttonText |> text ]
        , select [ onInput ChangePattern ]
            [ option [ value "glider" ] [ text "Glider" ]
            , option [ value "blinkers" ] [ text "Blinker" ]
            , option [ value "random" ] [ text "Random" ]
            ]
        , button
            [ style "margin" "10px"
            , onClick (ChangePattern selectedPattern)
            ]
            [ text "Reset" ]
        , svg
            [ width viewBoxSize, height viewBoxSize, viewBox viewBoxParams ]
            (cellList generation (\id -> ClickEvent id))
        ]


subscriptions : Model -> Sub Msg
subscriptions { isTicking } =
    case isTicking of
        True -> Time.every tickRate Tick
        False -> Sub.none



fetchLiveCells : Model -> List Cell
fetchLiveCells { aliveCells, generation } =
    case aliveCells of
        [] -> getAliveCells generation
        _ -> aliveCells



buildNewGeneration : Matrix Cell -> List Cell -> ( Matrix Cell, List Cell )
buildNewGeneration generation live =
    List.map (cellNeighbours generation) live
        |> List.concat
        |> List.append live
        |> uniqueBy .id
        |> List.foldl
            (\cell ( resultGen, resultAlives ) ->
                let
                    fate =
                        detectFate generation cell

                    newLiveList =
                        if fate.alive then fate :: resultAlives else resultAlives

                    newGeneration =
                        Matrix.set cell.posX cell.posY fate resultGen
                in
                ( newGeneration, newLiveList )
            )
            ( generation, [] )

toggle : String -> Cell -> Cell
toggle id cell =
    if cell.id == id then
        { cell | alive = not cell.alive }

    else
        cell


buttonText : Bool -> String
buttonText isTicking =
    if isTicking then
        "Stop Game"

    else
        "Start Game"
