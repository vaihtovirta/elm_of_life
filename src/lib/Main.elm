module Main exposing (Model)

import Array exposing (Array)
import Browser
import CellUtils exposing (cellList)
import Html exposing (Html, a, button, div, h1, option, select, text)
import Html.Attributes exposing (class, href, style, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import List.Extra exposing (uniqueBy)
import Matrix exposing (Matrix)
import MatrixUtils exposing (cellNeighbours, detectFate, fieldMatrix, getAliveCells, isAlive, randomFieldMatrix)
import Msgs exposing (Msg(..))
import Random
import Time
import Types exposing (Cell)

viewBoxSize : String
viewBoxSize =
    "800"


viewBoxParams : String
viewBoxParams =
    "0 0 800 800"


type alias Model =
    { generation : Matrix Cell
    , selectedPattern : String
    , gameStatus : GameStatus
    , aliveCells : List Cell
    , speed : Float
    }


type GameStatus
    = Ticking
    | Paused


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
        glider =
            fieldMatrix "glider"
    in
    ( { generation = fieldMatrix "glider"
      , selectedPattern = "glider"
      , gameStatus = Paused
      , aliveCells = getAliveCells glider
      , speed = 100
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            handleTick model

        ClickCell id ->
            handleClick model id

        ToggleTicking ->
            handleTicking model

        SelectPattern "random" ->
            handleSelectRandomPattern model

        SelectPattern patternName ->
            handleSelectPattern model patternName

        RandomField seed ->
            handleRandomSeed model seed

        SelectSpeed speed ->
            handleSelectSpeed model speed


view : Model -> Html Msg
view { generation, gameStatus, selectedPattern } =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-sm-offset-4" ]
                [ h1 []
                    [ a [ href "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life" ] [ text "Conway's Game of Life" ]
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-offset-4 field" ]
                [ lazy viewCells generation ]
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-offset-4" ]
                [ button
                    [ onClick ToggleTicking
                    ]
                    [ gameStatus |> buttonText |> text ]
                ]
            , div [ class "col-sm-1" ]
                [ select [ class "pattern-select", onInput SelectPattern ]
                    [ option [ value "glider" ] [ text "Glider" ]
                    , option [ value "blinkers" ] [ text "Blinker" ]
                    , option [ value "random" ] [ text "Random" ]
                    ]
                ]
            , div [ class "col-sm-1" ]
                [ select [ class "pattern-select", onInput SelectSpeed ]
                    [ option [ value "slow" ] [ text "Slow" ]
                    , option [ value "normal" ] [ text "Normal" ]
                    , option [ value "fast" ] [ text "Fast" ]
                    ]
                ]
            , div [ class "col-sm-1" ]
                [ button
                    [ class "secondary"
                    , onClick (SelectPattern selectedPattern)
                    ]
                    [ text "Reset" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions { gameStatus, speed } =
    case gameStatus of
        Paused ->
            Sub.none

        Ticking ->
            Time.every speed Tick


fetchLiveCells : Model -> List Cell
fetchLiveCells { aliveCells, generation } =
    case aliveCells of
        [] ->
            getAliveCells generation

        _ ->
            aliveCells


evolve : Matrix Cell -> List Cell -> ( Matrix Cell, List Cell )
evolve generation live =
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
                        if fate.alive then
                            fate :: resultAlives

                        else
                            resultAlives

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


buttonText : GameStatus -> String
buttonText gameStatus =
    case gameStatus of
        Paused ->
            "Start Game"

        Ticking ->
            "Stop Game"


viewCells generation =
    div
        [ class "inner-field"
        , style "position" "relative"
        , style "width" "750px"
        , style "height" "750px"
        ]
        (cellList generation (\id -> ClickCell id))


-- Message handlers


handleTick : Model -> ( Model, Cmd Msg )
handleTick model =
    let
        live =
            fetchLiveCells model

        ( newGeneration, newLiveCells ) =
            evolve model.generation live

        gameStatus =
            case live of
                [] ->
                    Paused

                _ ->
                    Ticking
    in
    ( { model
        | generation = newGeneration
        , aliveCells = newLiveCells
        , gameStatus = gameStatus
      }
    , Cmd.none
    )


handleClick : Model -> String -> ( Model, Cmd Msg )
handleClick model id =
    let
        newGeneration =
            Matrix.map (toggle id) model.generation

        newAliveCells =
            getAliveCells newGeneration
    in
    ( { model | generation = newGeneration, aliveCells = newAliveCells }
    , Cmd.none
    )


handleTicking : Model -> ( Model, Cmd Msg )
handleTicking model =
    let
        gameStatus =
            case model.gameStatus of
                Paused ->
                    Ticking

                Ticking ->
                    Paused
    in
    ( { model | gameStatus = gameStatus }
    , Cmd.none
    )


handleSelectPattern : Model -> String -> ( Model, Cmd Msg )
handleSelectPattern model patternName =
    ( { model
        | gameStatus = Paused
        , selectedPattern = patternName
        , aliveCells = []
        , generation = fieldMatrix patternName
      }
    , Cmd.none
    )


handleSelectRandomPattern : Model -> ( Model, Cmd Msg )
handleSelectRandomPattern model =
    ( model
    , Random.weighted ( 20, True ) [ ( 80, False ) ]
        |> Random.list 2500
        |> Random.generate RandomField
    )


handleRandomSeed : Model -> List Bool -> ( Model, Cmd Msg )
handleRandomSeed model seed =
    ( { model
        | gameStatus = Paused
        , selectedPattern = "random"
        , aliveCells = []
        , generation = seed |> Array.fromList |> randomFieldMatrix
      }
    , Cmd.none
    )


handleSelectSpeed model speed =
    let
        numSpeed = case speed of
            "slow" -> 200
            "normal" -> 100
            "fast" -> 10
            _ -> 1
    in
    ( { model | speed = numSpeed }
    , Cmd.none
    )
