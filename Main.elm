module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (id, class, classList, style)
import Html.Events exposing (onClick)
import List.Extra as ListX
import Random exposing (Generator)
import Random.List
import Maybe.Extra as MaybeX
import Element
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Element.Events as Events
import Element.Border as Border
import Color


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( initModel, Random.generate OnShuffle shuffledCardsGenerator )


type alias Model =
    { board : List (List (List Card))
    , bonus : Card
    , score : Int
    , trashes : Int
    , selected : List Card
    , discarded : List Card
    , gameState : GameState
    }


type alias Card =
    { rank : Rank
    , suit : Suit
    }


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type Suit
    = Hearts
    | Clubs
    | Diamonds
    | Spades


type GameState
    = NewGame
    | Playing
    | GameOver
    | HandList


dummyCard : Card
dummyCard =
    Card Ace Spades


dummyRow : List (List Card)
dummyRow =
    [ [ dummyCard ] ]


type Msg
    = OnShuffle (List Card)
    | ToggleCard Card
    | Trash Card
    | SubmitHand (List Card)
    | Clear
    | Hint
    | StartGame
    | ShowHandList
    | ResumePlaying


initModel : Model
initModel =
    { board = boardFromDeck standardDeck
    , bonus = bonusFromDeck standardDeck
    , score = 0
    , trashes = 2
    , selected = []
    , discarded = []
    , gameState = NewGame
    }


boardFromDeck : List Card -> List (List (List Card))
boardFromDeck deck =
    ListX.init deck
        |> Maybe.withDefault (List.repeat 51 dummyCard)
        |> ListX.groupsOfVarying [ 8, 8, 8, 7, 6, 5, 4, 3, 2 ]
        |> ListX.groupsOf 3


bonusFromDeck : List Card -> Card
bonusFromDeck deck =
    ListX.last deck |> Maybe.withDefault dummyCard


standardDeck : List Card
standardDeck =
    ListX.lift2 (flip Card)
        [ Hearts, Clubs, Diamonds, Spades ]
        orderedRanks


loopedRanks : List Rank
loopedRanks =
    [ Ace
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    , Ace
    ]


orderedRanks : List Rank
orderedRanks =
    loopedRanks |> ListX.init |> Maybe.withDefault [ dummyCard.rank ]


clearStackBonuses : List (List Int)
clearStackBonuses =
    [ 150, 100, 50 ] |> List.repeat 3 |> ListX.transpose



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        selectedStacks =
            boardInSelection model.selected model.board

        lastCards =
            lastCardsInBoard model.board

        clearBonus =
            bonusScoreFromSelection selectedStacks lastCards clearStackBonuses
    in
        case msg of
            OnShuffle cards ->
                ( { model
                    | board = boardFromDeck cards
                    , bonus = bonusFromDeck cards
                  }
                , Cmd.none
                )

            ToggleCard card ->
                ( { model
                    | selected =
                        if List.member card model.selected then
                            ListX.remove card model.selected
                        else
                            card :: model.selected
                  }
                , Cmd.none
                )

            Trash card ->
                ( { model
                    | board = removeCardFromBoard card model.board
                    , selected = []
                    , trashes = model.trashes - 1
                    , discarded = card :: model.discarded
                    , score = model.score + clearBonus
                  }
                    |> updateGameState
                , Cmd.none
                )

            SubmitHand cards ->
                ( { model
                    | board = cards |> List.foldl removeCardFromBoard model.board
                    , trashes = min initModel.trashes (model.trashes + 1)
                    , selected = []
                    , score =
                        let
                            hand =
                                scoreHand model.selected model.bonus.suit
                        in
                            if hand.isBonus then
                                model.score + (hand.baseScore * 2) + clearBonus
                            else
                                model.score + hand.baseScore + clearBonus
                    , discarded = cards ++ model.discarded
                  }
                    |> updateGameState
                , Cmd.none
                )

            Clear ->
                ( { model | selected = [] }, Cmd.none )

            Hint ->
                ( { model
                    | selected =
                        let
                            scoredHands =
                                scoredHandsFromBoard model.board model.bonus.suit
                        in
                            if List.length scoredHands > 0 then
                                scoredHands |> bestHandFromScored |> .hand
                            else
                                []
                  }
                , Cmd.none
                )

            StartGame ->
                ( initModel |> updateGameState, Random.generate OnShuffle shuffledCardsGenerator )

            ShowHandList ->
                ( { model | gameState = HandList }, Cmd.none )

            ResumePlaying ->
                ( { model | gameState = Playing }, Cmd.none )


shuffledCardsGenerator : Generator (List Card)
shuffledCardsGenerator =
    Random.List.shuffle standardDeck


updateGameState : Model -> Model
updateGameState model =
    case model.gameState of
        Playing ->
            { model
                | gameState =
                    if noMoreMoves model then
                        GameOver
                    else
                        model.gameState
            }

        NewGame ->
            { model | gameState = Playing }

        _ ->
            model


noMoreMoves : Model -> Bool
noMoreMoves model =
    let
        scoredHands =
            scoredHandsFromBoard model.board model.bonus.suit
    in
        List.length scoredHands == 0 && model.trashes == 0


type alias ScoredHand =
    { hand : List Card
    , handName : String
    , baseScore : Int
    , isBonus : Bool
    , actualScore : Int
    }


scoreHand : List Card -> Suit -> ScoredHand
scoreHand cards bonus =
    let
        ( handName, baseScore ) =
            if pair cards then
                ( "Pair", 10 )
            else if threeCardStraight cards then
                ( "3-card Straight", 20 )
            else if threeOfAKind cards then
                ( "Three of a Kind", 30 )
            else if fiveCardStraight cards then
                ( "5-card Straight", 50 )
            else if fullHouse cards then
                ( "Full House", 70 )
            else if fiveCardFlush cards then
                ( "Flush", 90 )
            else if fourOfAKind cards then
                ( "Four of a Kind", 100 )
            else if straightFlush cards then
                ( "Straight Flush", 150 )
            else
                ( "No hand", 0 )

        isBonus =
            cards |> List.map .suit |> List.any (\suit -> suit == bonus)
    in
        { hand = cards
        , handName = handName
        , baseScore = baseScore
        , isBonus = isBonus
        , actualScore =
            if isBonus then
                baseScore * 2
            else
                baseScore
        }



-- VIEW


view : Model -> Html Msg
view model =
    case model.gameState of
        NewGame ->
            viewNewGame

        Playing ->
            viewPlaying model

        GameOver ->
            viewGameOver

        HandList ->
            viewHandList


viewNewGame : Html Msg
viewNewGame =
    Element.layout
        [ Background.color Color.darkGreen ]
    <|
        Input.button
            []
            { onPress = Just StartGame, label = Element.text "Start new game" }


viewHandList : Html Msg
viewHandList =
    Element.layout
        [ Background.color Color.darkGreen ]
    <|
        Element.column []
            [ Element.el [] <| Element.text "Hand list goes here"
            , Input.button [] { onPress = Just ResumePlaying, label = Element.text "Back to game" }
            ]



-- viewPlaying : Model -> Html Msg
-- viewPlaying model =
--     let
--         cardView =
--             viewCard model.selected model.board model.bonus.suit
--         stackView =
--             viewStack cardView
--     in
--         Html.div []
--             [ Html.div [ id "board" ] [ viewBoard stackView model.board ]
--             , viewPlayingInfo model
--             , viewPlayingActions model
--             , viewPlayingSidebar model
--             , viewPlayingDebugging model
--             ]


viewPlaying : Model -> Html Msg
viewPlaying model =
    let
        cardView =
            viewCard model.selected model.board model.bonus.suit

        stackView =
            viewStack cardView
    in
        Element.layout [] <|
            Element.row []
                [ Element.column []
                    [ viewPlayingInfo model
                    , viewBoard stackView model.board
                    , viewPlayingActions model
                    ]
                , viewPlayingSidebar model
                ]


viewPlayingInfo : Model -> Element.Element Msg
viewPlayingInfo model =
    if
        validHand model.selected
            && List.length (uniqueRows model.selected model.board)
            > 1
    then
        viewPlayingInfoHand model
    else if anyStraight model.selected && List.length model.selected == 4 then
        Element.text "3 or 5 Card Straights only"
    else if
        List.length (suitCounts model.selected)
            == 1
            && (List.length model.selected == 3 || List.length model.selected == 4)
    then
        Element.text "Flushes must be 5 cards"
    else if rankCounts model.selected == [ 2, 2 ] then
        Element.text "Two Pair isn't a hand in Sage :("
    else
        Input.button [] { onPress = Just ShowHandList, label = Element.text "Show Hand List" }


viewGameOver : Html Msg
viewGameOver =
    Html.div []
        [ Html.div [] [ Html.text "Game over - thanks for playing" ]
        , viewNewGame
        ]


viewPlayingInfoHand : Model -> Element.Element Msg
viewPlayingInfoHand model =
    let
        hand =
            scoreHand model.selected model.bonus.suit

        bonusText =
            if hand.isBonus then
                " x2"
            else
                ""
    in
        Element.text <|
            hand.handName
                ++ " ("
                ++ toString hand.baseScore
                ++ " pts"
                ++ bonusText
                ++ ")"


viewPlayingActions : Model -> Element.Element Msg
viewPlayingActions model =
    if List.length model.selected == 1 && model.trashes > 0 then
        viewPlayingActionTrash model
    else if validHand model.selected && List.length (uniqueRows model.selected model.board) > 1 then
        viewPlayingActionCashIn model
    else
        Element.empty


viewPlayingActionTrash : Model -> Element.Element Msg
viewPlayingActionTrash model =
    let
        card =
            List.head model.selected |> Maybe.withDefault dummyCard
    in
        Input.button [] { onPress = Just (Trash card), label = Element.text "Trash" }


viewPlayingActionCashIn : Model -> Element.Element Msg
viewPlayingActionCashIn model =
    Input.button [] { onPress = Just (SubmitHand model.selected), label = Element.text "Cash In" }



-- viewPlayingSidebar : Model -> Html Msg
-- viewPlayingSidebar model =
--     Html.div []
--         [ Html.p [ id "score" ] [ Html.text <| "Score: " ++ toString model.score ]
--         , Html.button [ id "hint", onClick Hint ] [ Html.text "Hint" ]
--         , Html.div [ id "bonus" ] [ Html.text <| "Bonus: " ++ toString model.bonus ]
--         , Html.p [ id "trashes" ] [ Html.text <| "Trashes: " ++ toString model.trashes ]
--         , Html.p [ id "discarded" ] [ Html.text <| "Discarded: " ++ toString (List.length model.discarded) ]
--         , if List.length model.selected > 0 then
--             Html.button [ onClick Clear ] [ Html.text "Clear" ]
--           else
--             Html.text ""
--         ]


viewPlayingSidebar : Model -> Element.Element Msg
viewPlayingSidebar model =
    Element.column []
        [ Element.el [] <| Element.text <| "Score: " ++ toString model.score
        , Input.button [] { onPress = Just Hint, label = Element.text "Hint" }
        , Element.el [] <| Element.text <| "Bonus: " ++ toString model.bonus
        , Element.el [] <| Element.text <| "Trashes: " ++ toString model.trashes
        , Element.el [] <| Element.text <| "Discarded: " ++ toString (List.length model.discarded)
        , if List.length model.selected > 0 then
            Input.button [] { onPress = Just Clear, label = Element.text "Clear" }
          else
            Element.empty
        ]


viewPlayingDebugging : Model -> Html Msg
viewPlayingDebugging model =
    let
        scoredHands =
            scoredHandsFromBoard model.board model.bonus.suit

        selectedStacks =
            boardInSelection model.selected model.board

        lastCards =
            lastCardsInBoard model.board
    in
        Html.div []
            [ Html.p [] [ Html.text <| "Possible scores: " ++ toString (List.map .actualScore scoredHands) ]
            , Html.p [] [ Html.text <| "Best score: " ++ toString (.actualScore <| bestHandFromScored scoredHands) ]
            , Html.p [] [ Html.text <| "Discarded: " ++ toString model.discarded ]
            , Html.p [] [ Html.text <| "Game state: " ++ toString model.gameState ]
            , Html.p [] [ Html.text <| "No more moves: " ++ toString (noMoreMoves model) ]
            , Html.p [] [ Html.text <| "Suits selected: " ++ toString (suitCounts model.selected) ]
            , Html.p [] [ Html.text <| "Ranks selected: " ++ toString (rankCounts model.selected) ]
            , Html.p [] [ Html.text <| "Last cards: " ++ toString (lastCardsInBoard model.board) ]
            , Html.p [] [ Html.text <| "Selected: " ++ toString (boardInSelection model.selected model.board) ]
            , Html.p [] [ Html.text <| "Bonus: " ++ toString (bonusScoreFromSelection selectedStacks lastCards clearStackBonuses) ]
            ]


scoredHandsFromBoard : List (List (List Card)) -> Suit -> List ScoredHand
scoredHandsFromBoard board bonus =
    let
        validHands =
            board
                |> List.concat
                |> List.map List.head
                |> MaybeX.values
                |> ListX.subsequences
                |> List.filter
                    (\hand ->
                        validHand hand
                            && (List.length (uniqueRows hand board) > 1)
                    )
    in
        validHands |> List.map (\hand -> scoreHand hand bonus)


bestHandFromScored : List ScoredHand -> ScoredHand
bestHandFromScored hands =
    let
        maxScore =
            hands |> List.map .actualScore |> List.maximum |> Maybe.withDefault 0

        highScoringHands =
            hands
                |> List.filter (\hand -> hand.actualScore == maxScore)

        maxLengthHighScorers =
            highScoringHands
                |> List.map .hand
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 0

        longestHighScorers =
            highScoringHands
                |> List.filter (\hand -> List.length hand.hand == maxLengthHighScorers)
    in
        longestHighScorers
            |> List.head
            |> Maybe.withDefault (ScoredHand [ dummyCard ] "Dummy" 0 False 0)


inRow : Card -> Int -> List (List Card) -> Maybe Int
inRow target rownum row =
    if List.member target (List.concat row) then
        Just rownum
    else
        Nothing


inRowAll : List (List (List Card)) -> Card -> List Int
inRowAll grid target =
    grid
        |> List.indexedMap (inRow target)
        |> List.filterMap identity


uniqueRows : List Card -> List (List (List Card)) -> List Int
uniqueRows searches grid =
    searches
        |> List.concatMap (inRowAll grid)
        |> ListX.unique


viewBoard : (List Card -> Element.Element Msg) -> List (List (List Card)) -> Element.Element Msg
viewBoard stackView board =
    let
        viewRow : Int -> Element.Element Msg
        viewRow y =
            ListX.getAt y board
                |> Maybe.withDefault dummyRow
                |> List.map stackView
                |> Element.row []
    in
        List.range 0 (List.length board - 1)
            |> List.map viewRow
            |> Element.column []


viewStack : (Card -> Element.Element Msg) -> List Card -> Element.Element Msg
viewStack cardView cards =
    let
        stackAtts =
            [ Element.width <| Element.px 150
            , Element.height <| Element.px 150
            , Border.solid
            , Border.width 1
            , Border.color <| Color.rgb 0 204 68
            , Background.color <| Color.rgb 0 153 51
            , Element.paddingXY 15 25
            ]
    in
        case List.head cards of
            Nothing ->
                Element.el stackAtts Element.empty

            Just card ->
                let
                    cardString =
                        String.concat <| List.repeat (List.length cards - 1) "🂠"
                in
                    Element.el stackAtts <|
                        Element.column []
                            [ cardView card
                            , Element.el [] <| Element.text cardString
                            ]


viewCard : List Card -> List (List (List Card)) -> Suit -> Card -> Element.Element Msg
viewCard selected board bonus card =
    let
        selectionColor =
            if validHand selected && List.length (uniqueRows selected board) > 1 then
                Color.yellow
            else
                Color.red
    in
        Element.row
            [ Events.onClick (ToggleCard card)
            , Element.width <| Element.px 50
            , Element.height <| Element.px 70
            , Background.color Color.white
            , Border.rounded 5
            , Border.width 1
            , Border.solid
            , Border.color Color.grey
            , if List.member card selected then
                Border.glow selectionColor 3
              else
                Border.glow Color.white 0
            ]
            [ viewRank card.rank
            , viewSuit card.suit
            , viewBonusStar bonus card.suit
            ]





stackInSelection : List Card -> List Card -> Bool
stackInSelection selection cards =
    let
        topCard =
            List.head cards |> Maybe.withDefault dummyCard
    in
        List.member topCard selection


rowInSelection : List Card -> List (List Card) -> List Bool
rowInSelection selection row =
    row |> List.map (stackInSelection selection)


boardInSelection : List Card -> List (List (List Card)) -> List (List Bool)
boardInSelection selection board =
    board |> List.map (rowInSelection selection)


bonusScoreFromSelection : List (List Bool) -> List (List Bool) -> List (List Int) -> Int
bonusScoreFromSelection selected lastCard bonus =
    let
        selections =
            List.concat selected

        lastCards =
            List.concat lastCard

        bonuses =
            List.concat bonus
    in
        List.map3
            (\sel lc bon ->
                if sel && lc then
                    bon
                else
                    0
            )
            selections
            lastCards
            bonuses
            |> List.sum


lastCardInStack : List Card -> Bool
lastCardInStack cards =
    List.length cards == 1


lastCardsInRow : List (List Card) -> List Bool
lastCardsInRow row =
    List.map lastCardInStack row


lastCardsInBoard : List (List (List Card)) -> List (List Bool)
lastCardsInBoard board =
    List.map lastCardsInRow board


removeCardFromStack : Card -> List Card -> List Card
removeCardFromStack card stack =
    ListX.remove card stack


removeCardFromRow : Card -> List (List Card) -> List (List Card)
removeCardFromRow card row =
    row |> List.map (removeCardFromStack card)


removeCardFromBoard : Card -> List (List (List Card)) -> List (List (List Card))
removeCardFromBoard card board =
    board |> List.map (removeCardFromRow card)


validHand : List Card -> Bool
validHand sel =
    pair sel
        || threeCardStraight sel
        || threeOfAKind sel
        || fiveCardStraight sel
        || fullHouse sel
        || fiveCardFlush sel
        || fourOfAKind sel
        || straightFlush sel


handCounts : List Card -> (Card -> a) -> (a -> Int) -> List Int
handCounts selection component sortfunc =
    selection
        |> List.map component
        |> List.sortBy sortfunc
        |> ListX.group
        |> List.map List.length


rankCounts : List Card -> List Int
rankCounts sel =
    handCounts sel .rank rankToInt


suitCounts : List Card -> List Int
suitCounts sel =
    handCounts sel .suit suitToInt


anyStraight : List Card -> Bool
anyStraight sel =
    sel
        |> List.map .rank
        |> ListX.permutations
        |> List.any (flip ListX.isInfixOf loopedRanks)


pair : List Card -> Bool
pair sel =
    rankCounts sel == [ 2 ]


threeCardStraight : List Card -> Bool
threeCardStraight sel =
    List.length sel == 3 && anyStraight sel


threeOfAKind : List Card -> Bool
threeOfAKind sel =
    rankCounts sel == [ 3 ]


fiveCardStraight : List Card -> Bool
fiveCardStraight sel =
    List.length sel == 5 && anyStraight sel


fullHouse : List Card -> Bool
fullHouse sel =
    rankCounts sel == [ 2, 3 ] || rankCounts sel == [ 3, 2 ]


fiveCardFlush : List Card -> Bool
fiveCardFlush sel =
    List.length sel == 5 && List.length (suitCounts sel) == 1


fourOfAKind : List Card -> Bool
fourOfAKind sel =
    rankCounts sel == [ 4 ]


straightFlush : List Card -> Bool
straightFlush sel =
    fiveCardStraight sel && List.length (suitCounts sel) == 1


viewRank : Rank -> Element.Element Msg
viewRank rank =
    let
        r =
            case rank of
                Two ->
                    "2"

                Three ->
                    "3"

                Four ->
                    "4"

                Five ->
                    "5"

                Six ->
                    "6"

                Seven ->
                    "7"

                Eight ->
                    "8"

                Nine ->
                    "9"

                Ten ->
                    "10"

                _ ->
                    String.left 1 <| toString rank
    in
        Element.el [ Font.size 35, Font.bold ] <| Element.text r


viewSuit : Suit -> Element.Element Msg
viewSuit suit =
    let
        ( symbol, colour ) =
            case suit of
                Hearts ->
                    ( "♥", Color.red )

                Clubs ->
                    ( "♣", Color.black )

                Diamonds ->
                    ( "♦", Color.red )

                Spades ->
                    ( "♠", Color.black )
    in
        Element.el [ Font.color colour, Font.size 30 ] <| Element.text symbol


viewBonusStar : Suit -> Suit -> Element.Element Msg
viewBonusStar bonusSuit cardSuit =
    if cardSuit == bonusSuit then
        Element.el
            [ Font.color Color.orange ]
        <|
            Element.text "★"
    else
        Element.empty


rankToInt : Rank -> Int
rankToInt rank =
    ListX.elemIndex rank orderedRanks |> Maybe.withDefault 1


suitToInt : Suit -> Int
suitToInt suit =
    case suit of
        Hearts ->
            1

        Clubs ->
            2

        Diamonds ->
            3

        Spades ->
            4



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
