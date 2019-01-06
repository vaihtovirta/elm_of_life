module Main exposing (Model)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix)
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox)
import Time
import Debug exposing(log)

import Cell exposing (buildCellList)
import ItemModel exposing (Item)
import MatrixUtils exposing (fieldMatrix, isAlive)


viewBoxParams =
    "0 0 1000 1000"


type alias Model =
    { generation : Matrix Item, isTicking : Bool }


type Msg
    = ClickEvent String
    | Tick Time.Posix
    | ToggleTicking


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { generation = fieldMatrix, isTicking = False }
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


view : Model -> Html Msg
view { generation, isTicking } =
    div []
        [ button
            [ style "margin" "10px"
            , onClick ToggleTicking
            ]
            [ isTicking |> buttonText |> text ]
        , svg
            [ viewBox viewBoxParams ]
            (buildCellList generation ClickEvent)
        ]


subscriptions : Model -> Sub Msg
subscriptions { isTicking } =
    if isTicking then
        Time.every 100 Tick

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
