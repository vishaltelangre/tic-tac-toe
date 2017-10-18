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
    , computerPlayer : Maybe Player
    , gameStatus : GameStatus
    }


init : ( Model, Cmd Msg )
init =
    { board = initBoard
    , currentPlayer = O
    , computerPlayer = Nothing
    , gameStatus = NotStarted
    }
        ! [ Random.generate SetInitialPlayer randomPlayerGenerator ]


initBoard : Board
initBoard =
    positions2D
        |> List.concat
        |> List.foldl (\position dict -> Dict.insert position Nothing dict)
            Dict.empty


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


randomPlayerGenerator : Random.Generator Player
randomPlayerGenerator =
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


isPlayerWinner : Player -> Board -> Bool
isPlayerWinner player board =
    matchingWinnerLinesForPlayer player board |> List.isEmpty |> not


isPlayerComputer : Player -> Model -> Bool
isPlayerComputer player { computerPlayer } =
    case computerPlayer of
        Just computerPlayer ->
            player == computerPlayer

        _ ->
            False



---- UPDATE ----


type Msg
    = NoOp
    | TwoPlayersGame
    | PlayWithComputer
    | SetInitialPlayer Player
    | OwnPositionAsComputer (List Position)
    | OwnPosition Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ currentPlayer, board, gameStatus } as model) =
    case msg of
        NoOp ->
            model ! []

        TwoPlayersGame ->
            let
                ( newModel, cmd ) =
                    init
            in
                { newModel | gameStatus = InProgress } ! [ cmd ]

        PlayWithComputer ->
            let
                ( newModel, cmd ) =
                    init
            in
                { newModel | gameStatus = InProgress, computerPlayer = Just X }
                    ! [ cmd ]

        SetInitialPlayer player ->
            let
                cmd =
                    randomListGenerator board
                        |> Random.generate OwnPositionAsComputer
            in
                { model | currentPlayer = player } ! [ cmd ]

        OwnPosition position ->
            case gameStatus of
                InProgress ->
                    let
                        newModel =
                            ownPosition position model |> updateGameStatus

                        cmd =
                            randomListGenerator newModel.board
                                |> Random.generate OwnPositionAsComputer
                    in
                        newModel ! [ cmd ]

                _ ->
                    model ! []

        OwnPositionAsComputer randomList ->
            let
                randomPosition =
                    notOwnedPositions board
                        |> randomNotOwnedPosition randomList
            in
                ownPositionAsComputer randomPosition model


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


ownPositionAsComputer : Maybe Position -> Model -> ( Model, Cmd Msg )
ownPositionAsComputer randomPosition ({ currentPlayer, gameStatus } as model) =
    let
        shouldTryOwningPosition =
            isPlayerComputer currentPlayer model
                && (gameStatus == InProgress)
    in
        case randomPosition of
            Just position ->
                if shouldTryOwningPosition then
                    update (OwnPosition position) model
                else
                    model ! []

            Nothing ->
                model ! []


updateGameStatus : Model -> Model
updateGameStatus ({ currentPlayer, board } as model) =
    let
        opponent =
            opponentPlayer currentPlayer

        hasNotOwnedPositions =
            notOwnedPositions board |> List.isEmpty |> not

        updatedStatus =
            if isPlayerWinner currentPlayer board then
                WonBy currentPlayer
            else if isPlayerWinner opponent board then
                WonBy opponent
            else if hasNotOwnedPositions then
                model.gameStatus
            else
                Drawn
    in
        { model | gameStatus = updatedStatus }


randomListGenerator : Board -> Random.Generator (List Position)
randomListGenerator board =
    let
        totalNotOwnedPositions =
            board |> notOwnedPositions |> List.length
    in
        totalNotOwnedPositions |> Random.int 0 |> Random.list totalNotOwnedPositions


randomNotOwnedPosition : List Position -> List Position -> Maybe Position
randomNotOwnedPosition randomList positions =
    List.map2 (,) randomList positions
        |> List.sortBy Tuple.first
        |> List.unzip
        |> Tuple.second
        |> List.head


notOwnedPositions : Board -> List Position
notOwnedPositions board =
    board |> Dict.filter (\_ owner -> owner == Nothing) |> Dict.keys


positionAndOwnerAtPossibleWinningLines : Board -> List (List ( Position, Maybe Player ))
positionAndOwnerAtPossibleWinningLines board =
    let
        positionAndOwnerAt position =
            ( position, ownerAtPosition position board )
    in
        possibleWinningPositionLines |> List.map (List.map <| positionAndOwnerAt)


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
viewBoard ({ currentPlayer, board } as model) =
    let
        positionsToHighlight =
            winningPositionsOfWinner model

        isComputer =
            isPlayerComputer currentPlayer model
    in
        board
            |> board2D
            |> List.map (\row -> viewRow row ( positionsToHighlight, isComputer ))
            |> table []


board2D : Board -> List (List ( Position, Maybe Player ))
board2D board =
    let
        positionAndOwnerAt position =
            ( position, ownerAtPosition position board )
    in
        positions2D |> List.map (List.map positionAndOwnerAt)


viewBoardHeader : Model -> Html Msg
viewBoardHeader ({ currentPlayer, gameStatus } as model) =
    let
        view_ =
            case gameStatus of
                InProgress ->
                    div []
                        [ viewPlayerStatus X (currentPlayer == X) (isPlayerComputer X model)
                        , viewPlayerStatus O (currentPlayer == O) (isPlayerComputer O model)
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
        div [ class "boardHeader" ] [ view_ ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    span [ class (playerCssClass (Just player)) ] [ text (toString player) ]


viewPlayerStatus : Player -> Bool -> Bool -> Html Msg
viewPlayerStatus player isCurrentPlayer isComputer =
    let
        activeClass =
            if isCurrentPlayer then
                " active"
            else
                ""

        alsoKnownAs =
            if isComputer then
                span [ class "alsoKnown" ] [ text "Computer" ]
            else
                text ""
    in
        case player of
            X ->
                span [ class ("playerStatus left " ++ activeClass) ]
                    [ viewPlayer player
                    , span [ class "arrow" ] [ text "◀︎" ]
                    , alsoKnownAs
                    ]

            O ->
                span [ class ("playerStatus right " ++ activeClass) ]
                    [ alsoKnownAs
                    , span [ class "arrow" ] [ text "▶" ]
                    , viewPlayer player
                    ]


viewBoardFooter : Model -> Html Msg
viewBoardFooter { gameStatus } =
    let
        newGameButtons =
            case gameStatus of
                InProgress ->
                    text ""

                _ ->
                    div []
                        [ div [] [ text "Play new game as: " ]
                        , buttonNewGame "2 Players" TwoPlayersGame
                        , buttonNewGame "With Computer" PlayWithComputer
                        ]
    in
        div [ class "boardFooter" ] [ newGameButtons ]


viewRow : List ( Position, Maybe Player ) -> ( List Position, Bool ) -> Html Msg
viewRow row additionalInfo =
    row |> List.map (\position -> viewPosition position additionalInfo) |> tr []


viewPosition : ( Position, Maybe Player ) -> ( List Position, Bool ) -> Html Msg
viewPosition ( position, owner ) ( positionsToHighlight, isComputer ) =
    let
        className =
            if List.member position positionsToHighlight then
                playerCssClass owner ++ " highlight"
            else
                playerCssClass owner

        msgToSend =
            if isComputer then
                NoOp
            else
                (OwnPosition position)
    in
        td [ class className, onClick msgToSend ] [ text (ownerName owner) ]


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


buttonNewGame : String -> Msg -> Html Msg
buttonNewGame buttonText msg =
    button [ class "newGame", onClick msg ] [ text buttonText ]


winningPositionsOfWinner : Model -> List Position
winningPositionsOfWinner { gameStatus, board } =
    case gameStatus of
        WonBy winner ->
            matchingWinnerLinesForPlayer winner board
                |> List.head
                |> Maybe.withDefault []
                |> List.map Tuple.first
                |> List.map identity

        _ ->
            []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
