module Main exposing (..)

import Html exposing (Html, text, div)


---- MODEL ----


type GameStatus
    = NotStarted
    | InProgress
    | Drawn
    | WonBy Player


type Player
    = X
    | O


type alias Cell =
    { owner : Maybe Player
    , location : CellLocation
    }


type alias CellLocation =
    { row : Int
    , column : Int
    }


type alias Board =
    List Cell


type alias Model =
    { board : Board
    , currentTurn : Player
    , gameStatus : GameStatus
    }


init : ( Model, Cmd Msg )
init =
    { board = initBoard
    , currentTurn = X
    , gameStatus = NotStarted
    }
        ! []


initBoard : Board
initBoard =
    let
        cellAt row col =
            CellLocation row col |> Cell Nothing
    in
        [ cellAt 0 0
        , cellAt 0 1
        , cellAt 0 2
        , cellAt 1 0
        , cellAt 1 1
        , cellAt 1 2
        , cellAt 2 0
        , cellAt 2 1
        , cellAt 2 2
        ]



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] [ text (toString model) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
