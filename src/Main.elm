module Main exposing (..)

import Html exposing (Html, text, div, table, tr, td, button, span, p)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)


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
    , currentPlayer : Player
    , gameStatus : GameStatus
    }


init : ( Model, Cmd Msg )
init =
    { board = initBoard
    , currentPlayer = O
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
    = NewGame
    | OwnCell Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            let
                ( newModel, cmd ) =
                    init
            in
                { newModel | gameStatus = InProgress } ! [ cmd ]

        OwnCell cell ->
            case model.gameStatus of
                InProgress ->
                    let
                        ( newBoard, nextPlayer ) =
                            ownCell cell model.currentPlayer model.board

                        updatedModel =
                            { model
                                | board = newBoard
                                , currentPlayer = nextPlayer
                            }
                    in
                        determineGameStatus updatedModel ! []

                _ ->
                    model ! []


ownCell : Cell -> Player -> Board -> ( Board, Player )
ownCell cell currentPlayer board =
    if canOwnCell cell currentPlayer then
        let
            updateCell cell_ =
                if cell.location == cell_.location then
                    { cell_ | owner = Just currentPlayer }
                else
                    cell_
        in
            ( List.map updateCell board, flipPlayer currentPlayer )
    else
        ( board, currentPlayer )


flipPlayer : Player -> Player
flipPlayer currentPlayer =
    case currentPlayer of
        X ->
            O

        O ->
            X


canOwnCell : Cell -> Player -> Bool
canOwnCell cell currentPlayer =
    case cell.owner of
        Just _ ->
            False

        Nothing ->
            True


determineGameStatus : Model -> Model
determineGameStatus model =
    let
        opponent =
            flipPlayer model.currentPlayer

        owners =
            winningMoves
                |> List.map (cellOwnerAtLocation model.board |> List.map)

        hasUnownedCells =
            owners |> List.concat |> List.member Nothing

        expectedWinnerLine player =
            List.repeat (numberOfRows model.board) (Just player)

        isWinner player =
            owners
                |> List.filter (\line -> line == expectedWinnerLine player)
                |> List.isEmpty
                |> not

        newStatus =
            if isWinner model.currentPlayer then
                WonBy model.currentPlayer
            else if isWinner opponent then
                WonBy opponent
            else if hasUnownedCells then
                model.gameStatus
            else
                Drawn
    in
        { model | gameStatus = newStatus }


winningMoves : List (List CellLocation)
winningMoves =
    [ [ CellLocation 0 0, CellLocation 0 1, CellLocation 0 2 ]
    , [ CellLocation 1 0, CellLocation 1 1, CellLocation 1 2 ]
    , [ CellLocation 2 0, CellLocation 2 1, CellLocation 2 2 ]
    , [ CellLocation 0 0, CellLocation 1 0, CellLocation 2 0 ]
    , [ CellLocation 0 1, CellLocation 1 1, CellLocation 2 1 ]
    , [ CellLocation 0 2, CellLocation 1 2, CellLocation 2 2 ]
    , [ CellLocation 0 0, CellLocation 1 1, CellLocation 2 2 ]
    , [ CellLocation 0 2, CellLocation 1 1, CellLocation 2 0 ]
    ]


cellOwnerAtLocation : Board -> CellLocation -> Maybe Player
cellOwnerAtLocation board location =
    case (cellAtLocation board location) of
        Just cell ->
            cell.owner

        Nothing ->
            Nothing


cellAtLocation : Board -> CellLocation -> Maybe Cell
cellAtLocation board location =
    board
        |> List.filter (\cell -> cell.location == location)
        |> List.head



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewBoardHeader model
        , viewBoard model.board
        , viewBoardFooter model
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    board |> board2D |> List.map viewRow |> table []


viewBoardHeader : Model -> Html Msg
viewBoardHeader model =
    let
        view_ =
            case model.gameStatus of
                InProgress ->
                    span []
                        [ text "Turn of "
                        , viewPlayer model.currentPlayer
                        , text " player!"
                        ]

                Drawn ->
                    text "Game is drawn!"

                WonBy player ->
                    span []
                        [ text "Player "
                        , viewPlayer player
                        , text " is winner! 🎉"
                        ]

                NotStarted ->
                    text ""
    in
        p [ class "boardHeader" ]
            [ view_ ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    span
        [ class (playerCssClass (Just player)) ]
        [ text (playerName player) ]


viewBoardFooter : Model -> Html Msg
viewBoardFooter model =
    p [ class "boardFooter" ] [ buttonNewGame model.gameStatus ]


board2D : Board -> List (List Cell)
board2D board =
    let
        rowIndices =
            List.range 0 ((numberOfRows board) - 1)

        cellsInRow row =
            List.filter (\cell -> row == cell.location.row) board
    in
        List.map cellsInRow rowIndices


numberOfRows : Board -> Int
numberOfRows board =
    List.length board |> toFloat |> sqrt |> round


viewRow : List Cell -> Html Msg
viewRow row =
    List.map viewCell row |> tr []


viewCell : Cell -> Html Msg
viewCell cell =
    td
        [ class (playerCssClass cell.owner)
        , onClick (OwnCell cell)
        ]
        [ text (cellOwnerName cell.owner) ]


cellOwnerName : Maybe Player -> String
cellOwnerName owner =
    case owner of
        Just player ->
            playerName player

        Nothing ->
            ""


viewCurrentPlayer : Player -> Html Msg
viewCurrentPlayer player =
    span []
        [ text "Turn of "
        , span
            [ class (playerCssClass (Just player)) ]
            [ text (playerName player) ]
        , text " player!"
        ]


playerName : Player -> String
playerName player =
    case player of
        X ->
            "X"

        O ->
            "O"


playerCssClass : Maybe Player -> String
playerCssClass player =
    case player of
        Just player_ ->
            "player player" ++ playerName player_

        Nothing ->
            ""


buttonNewGame : GameStatus -> Html Msg
buttonNewGame gameStatus =
    case gameStatus of
        InProgress ->
            text ""

        _ ->
            button
                [ class "newGame", onClick NewGame ]
                [ text "Start New Game" ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
