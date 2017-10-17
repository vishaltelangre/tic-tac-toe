module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, text, div, table, tr, td, button, span, p)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Random


---- MODEL ----


type GameStatus
    = NotStarted
    | InProgress
    | Drawn
    | WonBy Player


type Player
    = X
    | O


type alias Position =
    Int


type alias Board =
    Dict Position (Maybe Player)


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
        ! [ Random.generate SetInitialPlayer randomPlayer ]


initBoard : Board
initBoard =
    let
        addNotOwnedPosition position dict =
            Dict.insert position Nothing dict
    in
        positions2D
            |> List.concat
            |> List.foldl addNotOwnedPosition Dict.empty


positions2D : List (List Position)
positions2D =
    [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ] ]


possibleWinningPositionLines : List (List Position)
possibleWinningPositionLines =
    [ [ 0, 1, 2 ]
    , [ 3, 4, 5 ]
    , [ 6, 7, 8 ]
    , [ 0, 3, 6 ]
    , [ 1, 4, 7 ]
    , [ 2, 5, 8 ]
    , [ 0, 4, 8 ]
    , [ 2, 4, 6 ]
    ]


randomPlayer : Random.Generator Player
randomPlayer =
    Random.bool
        |> Random.map
            (\randomBool ->
                if randomBool then
                    X
                else
                    O
            )


opponentPlayer : Player -> Player
opponentPlayer player =
    case player of
        X ->
            O

        O ->
            X



---- UPDATE ----


type Msg
    = NewGame
    | SetInitialPlayer Player
    | OwnPosition Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            let
                ( newModel, cmd ) =
                    init
            in
                { newModel | gameStatus = InProgress } ! [ cmd ]

        SetInitialPlayer player ->
            { model | currentPlayer = player } ! []

        OwnPosition position ->
            case model.gameStatus of
                InProgress ->
                    (ownPosition position model |> updateGameStatus) ! []

                _ ->
                    model ! []


ownPosition : Position -> Model -> Model
ownPosition position ({ currentPlayer, board } as model) =
    case Dict.get position board of
        Just Nothing ->
            let
                ownerInBoard _ =
                    Just (Just currentPlayer)

                updatedBoard =
                    Dict.update position ownerInBoard board
            in
                { model
                    | board = updatedBoard
                    , currentPlayer = (opponentPlayer currentPlayer)
                }

        _ ->
            model


updateGameStatus : Model -> Model
updateGameStatus ({ currentPlayer, board } as model) =
    let
        opponent =
            opponentPlayer currentPlayer

        positionsAndOwners =
            positionAndOwnerAtPossibleWinningLines board

        hasNotOwnedCells =
            positionsAndOwners
                |> List.concat
                |> List.map Tuple.second
                |> List.member Nothing

        updatedStatus =
            if isPlayerWinner currentPlayer board then
                WonBy currentPlayer
            else if isPlayerWinner opponent board then
                WonBy opponent
            else if hasNotOwnedCells then
                model.gameStatus
            else
                Drawn
    in
        { model | gameStatus = updatedStatus }


winningPositionsOfWinner : Model -> List Position
winningPositionsOfWinner { gameStatus, board } =
    case gameStatus of
        WonBy winner ->
            matchingWinnerLinesForPlayer winner board
                |> List.concat
                |> List.map Tuple.first
                |> List.map identity

        _ ->
            []


isPlayerWinner : Player -> Board -> Bool
isPlayerWinner player board =
    matchingWinnerLinesForPlayer player board
        |> List.isEmpty
        |> not


positionAndOwnerAtPossibleWinningLines : Board -> List (List ( Position, Maybe Player ))
positionAndOwnerAtPossibleWinningLines board =
    let
        positionAndOwnAt position =
            ( position, ownerAtPosition position board )
    in
        possibleWinningPositionLines |> List.map (List.map <| positionAndOwnAt)


matchingWinnerLinesForPlayer : Player -> Board -> List (List ( Position, Maybe Player ))
matchingWinnerLinesForPlayer player board =
    let
        positionsAndOwners =
            positionAndOwnerAtPossibleWinningLines board

        expectedWinnerLine player =
            Just player |> List.repeat (List.length positions2D)

        matchesWinnerLine player line =
            expectedWinnerLine player == List.map Tuple.second line
    in
        positionsAndOwners |> List.filter (matchesWinnerLine player)


ownerAtPosition : Position -> Board -> Maybe Player
ownerAtPosition position board =
    case Dict.get position board of
        Just (Just player) ->
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
        positionsToHighlight =
            winningPositionsOfWinner model
    in
        model.board
            |> board2D
            |> List.map (viewRow positionsToHighlight)
            |> table []


board2D : Board -> List (List ( Position, Maybe Player ))
board2D board =
    let
        positionAndOwnerAt position =
            ( position, ownerAtPosition position board )
    in
        positions2D |> List.map (List.map positionAndOwnerAt)


viewBoardHeader : Model -> Html Msg
viewBoardHeader { currentPlayer, gameStatus } =
    let
        view_ =
            case gameStatus of
                InProgress ->
                    div []
                        [ viewPlayerStatus X (currentPlayer == X)
                        , viewPlayerStatus O (currentPlayer == O)
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
                    span [ class "banner" ] [ text "Tic Tac Toe" ]
    in
        div [ class "boardHeader" ]
            [ view_ ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    span
        [ class (playerCssClass (Just player)) ]
        [ text (toString player) ]


viewPlayerStatus : Player -> Bool -> Html Msg
viewPlayerStatus player current =
    let
        defaultWrapperClass =
            case player of
                X ->
                    "playerStatus left"

                O ->
                    "playerStatus right"

        wrapperClass =
            if current then
                defaultWrapperClass ++ " active"
            else
                defaultWrapperClass
    in
        case player of
            X ->
                span [ class wrapperClass ]
                    [ viewPlayer player
                    , span [ class "arrow" ] [ text "◀︎" ]
                    ]

            O ->
                span [ class wrapperClass ]
                    [ span [ class "arrow" ] [ text "▶" ]
                    , viewPlayer player
                    ]


viewBoardFooter : Model -> Html Msg
viewBoardFooter { gameStatus } =
    p [ class "boardFooter" ] [ buttonNewGame gameStatus ]


viewRow : List Position -> List ( Position, Maybe Player ) -> Html Msg
viewRow positionsToHighlight row =
    List.map (viewPosition positionsToHighlight) row |> tr []


viewPosition : List Position -> ( Position, Maybe Player ) -> Html Msg
viewPosition positionsToHighlight ( position, owner ) =
    let
        defaultClass =
            playerCssClass owner

        updatedClass =
            if List.member position positionsToHighlight then
                defaultClass ++ " highlight"
            else
                defaultClass
    in
        td
            [ class updatedClass
            , onClick (OwnPosition position)
            ]
            [ text (ownerName owner) ]


ownerName : Maybe Player -> String
ownerName owner =
    case owner of
        Just player ->
            toString player

        Nothing ->
            ""


playerCssClass : Maybe Player -> String
playerCssClass maybePlayer =
    case maybePlayer of
        Just player ->
            "player player" ++ toString player

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
