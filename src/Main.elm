module Main exposing (Model)

import Browser
import Array exposing (fromList)
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Matrix exposing (Matrix)
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox, height, width)
import Time
import Random

import CellUtils exposing (cellList)
import Types exposing (Cell)
import Msgs exposing (Msg(..))
import MatrixUtils exposing (fieldMatrix, randomFieldMatrix, isAlive)


viewBoxSize : String
viewBoxSize =
    "800"


viewBoxParams : String
viewBoxParams =
    "0 0 800 800"


tickRate : Float
tickRate =
    140


type alias Model =
    {
        generation : Matrix Cell,
        selectedPattern : String,
        isTicking : Bool
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
    ( { generation = fieldMatrix "glider"
      , selectedPattern = "glider"
      , isTicking = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | generation = Matrix.map (toggleLife model.generation) model.generation }
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
            (
                model
            ,
                Random.weighted (20, True) [ (80, False) ]
                    |> Random.list 2500
                    |> Random.generate RandomField
            )

        ChangePattern patternName ->
            ( {
                model | isTicking = False,
                selectedPattern = patternName,
                generation = fieldMatrix patternName
            },
            Cmd.none
            )

        RandomField seed ->
            ( {
                model | isTicking = False,
                selectedPattern = "random",
                generation = seed |> Array.fromList |> randomFieldMatrix
            },
            Cmd.none
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
    if isTicking then
        Time.every tickRate Tick

    else
        Sub.none


toggle : String -> Cell -> Cell
toggle id cell =
    if cell.id == id then
        { cell | alive = not cell.alive }

    else
        cell


toggleLife : Matrix Cell -> Cell -> Cell
toggleLife matrix cell =
    { cell | alive = isAlive matrix cell }


buttonText : Bool -> String
buttonText isTicking =
    if isTicking then
        "Stop Game"

    else
        "Start Game"
