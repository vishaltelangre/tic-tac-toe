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


type Cell
    = NotOwned CellLocation
    | OwnedBy CellLocation Player


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
            CellLocation row col |> NotOwned
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
                if (locationOfCell cell) == (locationOfCell cell_) then
                    OwnedBy (locationOfCell cell) currentPlayer
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
    case cell of
        NotOwned _ ->
            True

        _ ->
            False


determineGameStatus : Model -> Model
determineGameStatus model =
    let
        opponent =
            flipPlayer model.currentPlayer

        cellsAndOwners =
            cellAndOwnerAtPossibleWinningLines model.board

        hasNotOwnedCells =
            cellsAndOwners
                |> List.concat
                |> List.map Tuple.second
                |> List.member Nothing

        updatedStatus =
            if isPlayerWinner model.currentPlayer model.board then
                WonBy model.currentPlayer
            else if isPlayerWinner opponent model.board then
                WonBy opponent
            else if hasNotOwnedCells then
                model.gameStatus
            else
                Drawn
    in
        { model | gameStatus = updatedStatus }


winningCellsOfWinner : Model -> List Cell
winningCellsOfWinner model =
    case model.gameStatus of
        WonBy winner ->
            matchingWinnerLinesForPlayer winner model.board
                |> List.concat
                |> List.map Tuple.first
                |> List.filterMap identity

        _ ->
            []


isPlayerWinner : Player -> Board -> Bool
isPlayerWinner player board =
    matchingWinnerLinesForPlayer player board
        |> List.isEmpty
        |> not


cellAndOwnerAtPossibleWinningLines : Board -> List (List ( Maybe Cell, Maybe Player ))
cellAndOwnerAtPossibleWinningLines board =
    possibleWinningLines |> List.map (List.map <| cellAndOwnerAtLocation board)


matchingWinnerLinesForPlayer : Player -> Board -> List (List ( Maybe Cell, Maybe Player ))
matchingWinnerLinesForPlayer player board =
    let
        cellsAndOwners =
            cellAndOwnerAtPossibleWinningLines board

        expectedWinnerLine player =
            Just player |> List.repeat (numberOfRows board)

        matchesWinnerLine player line =
            expectedWinnerLine player == List.map Tuple.second line
    in
        cellsAndOwners |> List.filter (matchesWinnerLine player)


possibleWinningLines : List (List CellLocation)
possibleWinningLines =
    [ [ CellLocation 0 0, CellLocation 0 1, CellLocation 0 2 ]
    , [ CellLocation 1 0, CellLocation 1 1, CellLocation 1 2 ]
    , [ CellLocation 2 0, CellLocation 2 1, CellLocation 2 2 ]
    , [ CellLocation 0 0, CellLocation 1 0, CellLocation 2 0 ]
    , [ CellLocation 0 1, CellLocation 1 1, CellLocation 2 1 ]
    , [ CellLocation 0 2, CellLocation 1 2, CellLocation 2 2 ]
    , [ CellLocation 0 0, CellLocation 1 1, CellLocation 2 2 ]
    , [ CellLocation 0 2, CellLocation 1 1, CellLocation 2 0 ]
    ]


cellAndOwnerAtLocation : Board -> CellLocation -> ( Maybe Cell, Maybe Player )
cellAndOwnerAtLocation board location =
    case cellAtLocation board location of
        Just cell ->
            case cell of
                NotOwned _ ->
                    ( Just cell, Nothing )

                OwnedBy _ player ->
                    ( Just cell, Just player )

        Nothing ->
            ( Nothing, Nothing )


cellAtLocation : Board -> CellLocation -> Maybe Cell
cellAtLocation board location =
    let
        cellMatchesLocation cell =
            case cell of
                NotOwned cellLocation ->
                    location == cellLocation

                OwnedBy cellLocation _ ->
                    location == cellLocation
    in
        board |> List.filter cellMatchesLocation |> List.head


numberOfRows : Board -> Int
numberOfRows board =
    List.length board |> toFloat |> sqrt |> round


locationOfCell : Cell -> CellLocation
locationOfCell cell =
    case cell of
        NotOwned location ->
            location

        OwnedBy location _ ->
            location


ownerOfCell : Cell -> Maybe Player
ownerOfCell cell =
    case cell of
        OwnedBy _ player ->
            Just player

        _ ->
            Nothing



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewBoardHeader model
        , viewBoard model
        , viewBoardFooter model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        cellsToHighlight =
            winningCellsOfWinner model
    in
        model.board
            |> board2D
            |> List.map (viewRow cellsToHighlight)
            |> table []


board2D : Board -> List (List Cell)
board2D board =
    let
        rowIndices =
            List.range 0 ((numberOfRows board) - 1)

        cellsInRow rowIndex =
            board
                |> List.filter
                    (\cell -> rowIndex == (cell |> locationOfCell |> .row))
    in
        List.map cellsInRow rowIndices


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


viewRow : List Cell -> List Cell -> Html Msg
viewRow cellsToHighlight row =
    List.map (viewCell cellsToHighlight) row |> tr []


viewCell : List Cell -> Cell -> Html Msg
viewCell cellsToHighlight cell =
    let
        defaultClass =
            ownerOfCell cell |> playerCssClass

        updatedClass =
            if List.member cell cellsToHighlight then
                defaultClass ++ " highlight"
            else
                defaultClass
    in
        td
            [ class updatedClass
            , onClick (OwnCell cell)
            ]
            [ text (ownerOfCell cell |> cellOwnerName) ]


cellOwnerName : Maybe Player -> String
cellOwnerName owner =
    case owner of
        Just player ->
            playerName player

        Nothing ->
            ""


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
