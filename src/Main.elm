module Main exposing (Model)

import Browser
import Html exposing (Html, button, div, option, select, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Matrix exposing (Matrix)
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox)
import Time

import Cell exposing (buildCellList)
import ItemModel exposing (Item)
import MatrixUtils exposing (fieldMatrix, isAlive)

viewBoxParams =
    "0 0 750 750"


type alias Model =
    { generation : Matrix Item, isTicking : Bool }


type Msg
    = ClickEvent String
    | Tick Time.Posix
    | ToggleTicking
    | ChangeSelect String


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

        ChangeSelect patternName ->
            ( { model | isTicking = False, generation = fieldMatrix patternName }
            , Cmd.none
            )


view : Model -> Html Msg
view { generation, isTicking } =
    div []
        [ button
            [ style "margin" "10px"
            , onClick ToggleTicking
            ]
            [ isTicking |> buttonText |> text ]
        , select [ onInput ChangeSelect ]
            [ option [ value "glider" ] [ text "Glider" ]
            , option [ value "blinkers" ] [ text "Blinker" ]
            ]
        , svg
            [ viewBox viewBoxParams ]
            (buildCellList generation ClickEvent)
        ]


subscriptions : Model -> Sub Msg
subscriptions { isTicking } =
    if isTicking then
        Time.every 140 Tick

    else
        Sub.none


toggle : String -> Item -> Item
toggle id item =
    if item.id == id then
        { item | alive = not item.alive }

    else
        item


toggleLife : Matrix Item -> Item -> Item
toggleLife matrix item =
    { item | alive = isAlive matrix item }


buttonText : Bool -> String
buttonText isTicking =
    if isTicking then
        "Stop Game"

    else
        "Start Game"
