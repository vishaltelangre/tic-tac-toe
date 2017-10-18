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
    , computer : Maybe Player
    , gameStatus : GameStatus
    }


init : ( Model, Cmd Msg )
init =
    { board = initBoard
    , currentPlayer = O
    , computer = Nothing
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


randomListGenerator : Board -> Random.Generator (List Position)
randomListGenerator board =
    let
        totalNotOwnedPositions =
            board |> notOwnedPositions |> List.length
    in
        Random.int 0 totalNotOwnedPositions
            |> Random.list totalNotOwnedPositions


opponentPlayer : Player -> Player
opponentPlayer player =
    case player of
        X ->
            O

        O ->
            X


isPlayerComputer : Player -> Model -> Bool
isPlayerComputer player { computer } =
    case computer of
        Just computerPlayer ->
            player == computerPlayer

        _ ->
            False



---- UPDATE ----


type Msg
    = NoOp
    | TwoPlayerGame
    | PlayWithComputer
    | SetInitialPlayer Player
    | TryOwningPositionAsComputer (List Position)
    | OwnPosition Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        TwoPlayerGame ->
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
                { newModel | gameStatus = InProgress, computer = Just X } ! [ cmd ]

        SetInitialPlayer player ->
            { model | currentPlayer = player }
                ! [ Random.generate TryOwningPositionAsComputer (randomListGenerator model.board) ]

        TryOwningPositionAsComputer randomList ->
            case model.gameStatus of
                InProgress ->
                    case randomPosition randomList (notOwnedPositions model.board) of
                        Just position ->
                            if isPlayerComputer model.currentPlayer model then
                                update (OwnPosition position) model
                            else
                                model ! []

                        Nothing ->
                            model ! []

                _ ->
                    model ! []

        OwnPosition position ->
            case model.gameStatus of
                InProgress ->
                    let
                        newModel =
                            (ownPosition position model |> updateGameStatus)
                    in
                        newModel
                            ! [ Random.generate TryOwningPositionAsComputer (randomListGenerator newModel.board) ]

                _ ->
                    model ! []


randomPosition : List Position -> List Position -> Maybe Position
randomPosition randomList positions =
    List.map2 (,) randomList positions
        |> List.sortBy Tuple.first
        |> List.unzip
        |> Tuple.second
        |> List.head


notOwnedPositions : Board -> List Position
notOwnedPositions board =
    board
        |> Dict.filter (\_ owner -> owner == Nothing)
        |> Dict.keys


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
                |> List.head
                |> Maybe.withDefault []
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

        isComputer =
            isPlayerComputer model.currentPlayer model
    in
        model.board
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
        div [ class "boardHeader" ]
            [ view_ ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    span
        [ class (playerCssClass (Just player)) ]
        [ text (toString player) ]


viewPlayerStatus : Player -> Bool -> Bool -> Html Msg
viewPlayerStatus player isCurrent isComputer =
    let
        defaultWrapperClass =
            case player of
                X ->
                    "playerStatus left"

                O ->
                    "playerStatus right"

        wrapperClass =
            if isCurrent then
                defaultWrapperClass ++ " active"
            else
                defaultWrapperClass

        alsoKnownAs =
            if isComputer then
                "Computer"
            else
                "You"
    in
        case player of
            X ->
                span [ class wrapperClass ]
                    [ viewPlayer player
                    , span [ class "arrow" ] [ text "◀︎" ]
                    , span [] [ text alsoKnownAs ]
                    ]

            O ->
                span [ class wrapperClass ]
                    [ span [] [ text alsoKnownAs ]
                    , span [ class "arrow" ] [ text "▶" ]
                    , viewPlayer player
                    ]


viewBoardFooter : Model -> Html Msg
viewBoardFooter { gameStatus } =
    let
        buttons =
            case gameStatus of
                InProgress ->
                    text ""

                _ ->
                    div []
                        [ text "Play As: "
                        , buttonNewGame "2 Players" TwoPlayerGame
                        , buttonNewGame "With Computer" PlayWithComputer
                        ]
    in
        div [ class "boardFooter" ] [ buttons ]


viewRow : List ( Position, Maybe Player ) -> ( List Position, Bool ) -> Html Msg
viewRow row additionalInfo =
    row
        |> List.map (\position -> viewPosition position additionalInfo)
        |> tr []


viewPosition : ( Position, Maybe Player ) -> ( List Position, Bool ) -> Html Msg
viewPosition ( position, owner ) ( positionsToHighlight, isComputer ) =
    let
        defaultClass =
            playerCssClass owner

        updatedClass =
            if List.member position positionsToHighlight then
                defaultClass ++ " highlight"
            else
                defaultClass

        messageToDispatch =
            if isComputer then
                NoOp
            else
                (OwnPosition position)
    in
        td
            [ class updatedClass
            , onClick messageToDispatch
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


buttonNewGame : String -> Msg -> Html Msg
buttonNewGame buttonText msg =
    button
        [ class "newGame", onClick msg ]
        [ text buttonText ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
