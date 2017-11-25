module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, text, div, table, tr, td, button, span, p)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Process
import Random
import Task
import Time exposing (Time)


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


type alias Move =
    { position : Maybe Position, score : Int }


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
    | SetComputerPlayer Player
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
                    Random.generate SetComputerPlayer randomPlayerGenerator
            in
                { model | currentPlayer = player } ! [ cmd ]

        SetComputerPlayer player ->
            let
                cmd =
                    randomListGenerator board
                        |> Random.generate OwnPositionAsComputer
            in
                case model.computerPlayer of
                    Just _ ->
                        { model | computerPlayer = Just player } ! [ cmd ]

                    Nothing ->
                        model ! [ cmd ]

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
            notOwnedPositions board
                |> maybeRandomAvailablePosition randomList
                |> ownPositionAsComputer model


ownPosition : Position -> Model -> Model
ownPosition position ({ currentPlayer, board } as model) =
    case Dict.get position board of
        Just Nothing ->
            { model
                | board = Dict.update position (currentPlayer |> Just |> Just |> always) board
                , currentPlayer = (opponentPlayer currentPlayer)
            }

        _ ->
            model


ownPositionAsComputer : Model -> Maybe Position -> ( Model, Cmd Msg )
ownPositionAsComputer ({ currentPlayer, gameStatus, board } as model) maybeRandomPosition =
    case maybeRandomPosition of
        Just position ->
            let
                shouldTryOwningPosition =
                    isPlayerComputer currentPlayer model
                        && (gameStatus == InProgress)
            in
                if shouldTryOwningPosition then
                    let
                        msg =
                            OwnPosition (nextPositionChosenByComputer position model)

                        cmd =
                            randomDelay position |> Process.sleep |> Task.perform (always msg)
                    in
                        model ! [ cmd ]
                else
                    model ! []

        Nothing ->
            model ! []


nextPositionChosenByComputer : Position -> Model -> Position
nextPositionChosenByComputer defaultPosition ({ board, currentPlayer } as model) =
    let
        {--
            Let the computer make an intelligent move if the
            available number of positions are between 3 and 7!

            Do you think that the computer is that much dumb now, huh?
        --}
        randomThreshold =
            Random.initialSeed defaultPosition
                |> Random.step (Random.int 3 7)
                |> Tuple.first

        shouldMakeSmartMove =
            (notOwnedPositions board |> List.length) <= randomThreshold
    in
        if shouldMakeSmartMove then
            case minimax model currentPlayer of
                Just { position } ->
                    Maybe.withDefault defaultPosition position

                _ ->
                    defaultPosition
        else
            defaultPosition


minimax : Model -> Player -> Maybe Move
minimax ({ board } as model) player =
    let
        availablePositions =
            notOwnedPositions board

        moveTo position =
            Move (Just position) (lookAheadScoreAtPosition position model player)

        moves =
            availablePositions |> List.map moveTo |> List.sortBy .score
    in
        if isPlayerWinner player board then
            if isPlayerComputer player model then
                Just (Move Nothing 1)
            else
                Just (Move Nothing -1)
        else if availablePositions |> List.isEmpty then
            Just (Move Nothing 0)
        else if isPlayerComputer player model then
            moves |> List.reverse |> List.head
        else
            moves |> List.head


lookAheadScoreAtPosition : Position -> Model -> Player -> Int
lookAheadScoreAtPosition position ({ board } as model) player =
    let
        newBoard =
            Dict.update position (player |> Just |> Just |> always) board
    in
        minimax { model | board = newBoard } (opponentPlayer player)
            |> Maybe.withDefault (Move (Just position) 0)
            |> .score


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


maybeRandomAvailablePosition : List Position -> List Position -> Maybe Position
maybeRandomAvailablePosition randomList positions =
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


randomDelay : Int -> Time
randomDelay randomSeed =
    Random.initialSeed randomSeed
        |> Random.step (Random.float 1 2)
        |> Tuple.first
        |> (*) Time.second



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
                    let
                        playerPrefix =
                            if isPlayerComputer player model then
                                "Computer "
                            else
                                " Player "
                    in
                        span [] [ text playerPrefix, viewPlayer player, text " is winner! 🎉" ]

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
        ( activeClass, thinkingOrNot ) =
            if isCurrentPlayer then
                ( " active", " thinking" )
            else
                ( "", "" )

        alsoKnownAs =
            if isComputer then
                span [ class "alsoKnown" ] [ text ("Computer" ++ thinkingOrNot) ]
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
