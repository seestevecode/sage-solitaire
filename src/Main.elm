module Main exposing (main)

import Html exposing (Html)
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
import Bool.Extra as BoolX


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
    ( initModel, Cmd.none )


type alias Model =
    { board : Board
    , bonus : Card
    , score : Int
    , trashes : Int
    , selected : List Card
    , discarded : List Card
    , gameState : GameState
    , hintsUsed : Int
    }


type alias Board =
    List (List (List Card))


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
    , hintsUsed = 0
    }


boardFromDeck : List Card -> Board
boardFromDeck deck =
    ListX.init deck
        |> Maybe.withDefault (List.repeat 51 dummyCard)
        |> ListX.groupsOfVarying [ 8, 8, 8, 7, 6, 5, 4, 3, 2 ]
        |> ListX.groupsOf 3


dummyCard : Card
dummyCard =
    Card Ace Spades


standardDeck : List Card
standardDeck =
    ListX.lift2 (flip Card)
        [ Hearts, Clubs, Diamonds, Spades ]
        orderedRanks


orderedRanks : List Rank
orderedRanks =
    loopedRanks |> ListX.init |> Maybe.withDefault [ dummyCard.rank ]


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


bonusFromDeck : List Card -> Card
bonusFromDeck deck =
    ListX.last deck |> Maybe.withDefault dummyCard


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
                    , score =
                        model.score - hintCost model.score model.hintsUsed
                  }
                , Cmd.none
                )

            StartGame ->
                ( initModel
                    |> updateGameState
                , Random.generate OnShuffle shuffledCardsGenerator
                )

            ShowHandList ->
                ( { model | gameState = HandList }, Cmd.none )

            ResumePlaying ->
                ( { model | gameState = Playing }, Cmd.none )


boardInSelection : List Card -> Board -> List (List Bool)
boardInSelection selection board =
    board |> List.map (rowInSelection selection)


rowInSelection : List Card -> List (List Card) -> List Bool
rowInSelection selection row =
    row |> List.map (stackInSelection selection)


stackInSelection : List Card -> List Card -> Bool
stackInSelection selection cards =
    let
        topCard =
            List.head cards |> Maybe.withDefault dummyCard
    in
        List.member topCard selection


lastCardsInBoard : Board -> List (List Bool)
lastCardsInBoard board =
    List.map lastCardsInRow board


lastCardsInRow : List (List Card) -> List Bool
lastCardsInRow row =
    List.map lastCardInStack row


lastCardInStack : List Card -> Bool
lastCardInStack cards =
    List.length cards == 1


bonusScoreFromSelection :
    List (List Bool)
    -> List (List Bool)
    -> List (List Int)
    -> Int
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


removeCardFromBoard : Card -> Board -> Board
removeCardFromBoard card board =
    board |> List.map (removeCardFromRow card)


removeCardFromRow : Card -> List (List Card) -> List (List Card)
removeCardFromRow card row =
    row |> List.map (removeCardFromStack card)


removeCardFromStack : Card -> List Card -> List Card
removeCardFromStack card stack =
    ListX.remove card stack


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


scoreHand : List Card -> Suit -> ScoredHand
scoreHand cards bonus =
    let
        validHand =
            validGameHands
                |> List.filter (flip .handCheck cards)
                |> List.head
                |> Maybe.withDefault (Hand "No hand" (\_ -> False) 0)

        isBonus =
            cards |> List.map .suit |> List.any (\suit -> suit == bonus)
    in
        { hand = cards
        , handName = validHand.handName
        , baseScore = validHand.baseScore
        , isBonus = isBonus
        , actualScore =
            if isBonus then
                validHand.baseScore * 2
            else
                validHand.baseScore
        }


hintCost : Int -> Int -> Int
hintCost score hints =
    clamp 0 (min score (2 ^ hints * 10)) 150


validGameHands : List Hand
validGameHands =
    [ Hand "Straight Flush" straightFlush 150
    , Hand "Four Of A Kind" fourOfAKind 100
    , Hand "Flush" fiveCardFlush 90
    , Hand "Full House" fullHouse 70
    , Hand "5-Card Straight" fiveCardStraight 50
    , Hand "Three Of A Kind" threeOfAKind 30
    , Hand "3-Card Straight" threeCardStraight 20
    , Hand "Pair" pair 10
    ]


type alias Hand =
    { handName : String
    , handCheck : List Card -> Bool
    , baseScore : Int
    }


type alias ScoredHand =
    { hand : List Card
    , handName : String
    , baseScore : Int
    , isBonus : Bool
    , actualScore : Int
    }


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


anyStraight : List Card -> Bool
anyStraight sel =
    sel
        |> List.map .rank
        |> ListX.permutations
        |> List.any (flip ListX.isInfixOf loopedRanks)


rankCounts : List Card -> List Int
rankCounts sel =
    handCounts sel .rank rankToInt


suitCounts : List Card -> List Int
suitCounts sel =
    handCounts sel .suit suitToInt


handCounts : List Card -> (Card -> a) -> (a -> Int) -> List Int
handCounts selection component sortfunc =
    selection
        |> List.map component
        |> List.sortBy sortfunc
        |> ListX.group
        |> List.map List.length


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


scoredHandsFromBoard : Board -> Suit -> List ScoredHand
scoredHandsFromBoard board bonus =
    let
        validBoardHands =
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
        validBoardHands |> List.map (\hand -> scoreHand hand bonus)


validHand : List Card -> Bool
validHand sel =
    validGameHands
        |> List.map (flip .handCheck sel)
        |> BoolX.any


uniqueRows : List Card -> Board -> List Int
uniqueRows searches grid =
    searches
        |> List.concatMap (inRowAll grid)
        |> ListX.unique


inRowAll : Board -> Card -> List Int
inRowAll grid target =
    grid
        |> List.indexedMap (inRow target)
        |> List.filterMap identity


inRow : Card -> Int -> List (List Card) -> Maybe Int
inRow target rownum row =
    if List.member target (List.concat row) then
        Just rownum
    else
        Nothing


bestHandFromScored : List ScoredHand -> ScoredHand
bestHandFromScored hands =
    let
        maxScore =
            hands
                |> List.map .actualScore
                |> List.maximum
                |> Maybe.withDefault 0

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
                |> List.filter
                    (\hand ->
                        List.length hand.hand == maxLengthHighScorers
                    )
    in
        longestHighScorers
            |> List.head
            |> Maybe.withDefault (ScoredHand [ dummyCard ] "Dummy" 0 False 0)


shuffledCardsGenerator : Generator (List Card)
shuffledCardsGenerator =
    Random.List.shuffle standardDeck



-- VIEW


view : Model -> Html Msg
view model =
    let
        layout =
            case model.gameState of
                NewGame ->
                    viewNewGameButton

                Playing ->
                    viewPlaying model

                GameOver ->
                    viewGameOver model

                HandList ->
                    viewHandList
    in
        Element.layout [ Background.color Color.grey ] <|
            Element.el
                [ Background.color Color.darkGreen
                , Element.height <| Element.px 600
                , Element.width <| Element.px 600
                , Element.centerX
                , Element.centerY
                ]
                layout


viewNewGameButton : Element.Element Msg
viewNewGameButton =
    Input.button
        viewWhiteBarAtts
        { onPress = Just StartGame, label = Element.text "Start new game" }


viewWhiteBarAtts : List (Element.Attribute Msg)
viewWhiteBarAtts =
    [ Element.centerX
    , Element.width Element.fill
    , Background.color Color.white
    , Font.center
    , Element.paddingXY 0 10
    ]


viewPlaying : Model -> Element.Element Msg
viewPlaying model =
    let
        cardView =
            viewCard Board model.selected model.board model.bonus.suit

        stackView =
            viewStack cardView
    in
        Element.row
            [ Element.height <| Element.px 600 ]
            [ Element.el
                [ Element.paddingXY 0 20
                , Element.above <| viewPlayingInfo model
                , Element.below <| viewPlayingActions model
                ]
              <|
                viewBoard stackView model.board
            , viewPlayingSidebar model
            ]


viewCard :
    CardLocation
    -> List Card
    -> Board
    -> Suit
    -> Card
    -> Element.Element Msg
viewCard location selected board bonus card =
    let
        selectionColor =
            if
                validHand selected
                    && List.length (uniqueRows selected board)
                    > 1
            then
                Color.yellow
            else
                Color.red

        locationAtts =
            case location of
                Board ->
                    [ Events.onClick (ToggleCard card)
                    , Element.pointer
                    , if List.member card selected then
                        Border.glow selectionColor 3
                      else
                        Border.glow Color.white 0
                    ]

                Sidebar ->
                    [ Element.above <|
                        Element.el
                            [ Element.paddingXY 0 5
                            , Element.centerX
                            ]
                        <|
                            Element.text "Bonus suit"
                    ]
    in
        Element.el
            ([ Element.inFront <| viewRank card.rank
             , Element.inFront <| viewSuit card.suit
             , Element.inFront <| viewBonusStar bonus card.suit
             , Element.width <| Element.px 70
             , Element.height <| Element.px 100
             , Background.color Color.white
             , Border.rounded 6
             , Border.width 1
             , Border.solid
             , Border.color Color.darkGrey
             , Element.centerX
             ]
                ++ locationAtts
            )
            Element.empty


type CardLocation
    = Board
    | Sidebar


viewRank : Rank -> Element.Element Msg
viewRank rank =
    let
        rankNum =
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
        Element.el
            [ Element.alignLeft
            , Element.alignTop
            , Element.padding 5
            , Font.size 45
            , Font.bold
            ]
        <|
            Element.text rankNum


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
        Element.el
            [ Element.alignRight
            , Element.alignBottom
            , Element.padding 8
            , Element.moveDown 2
            , Font.color colour
            , Font.size 35
            ]
        <|
            Element.text symbol


viewBonusStar : Suit -> Suit -> Element.Element Msg
viewBonusStar bonusSuit cardSuit =
    if cardSuit == bonusSuit then
        Element.el
            [ Element.alignBottom
            , Element.alignLeft
            , Element.moveUp 2
            , Element.padding 5
            , Font.color Color.orange
            ]
        <|
            Element.text "★"
    else
        Element.empty


viewStack : (Card -> Element.Element Msg) -> List Card -> Element.Element Msg
viewStack cardView cards =
    let
        stackAtts =
            [ Element.width <| Element.px 150
            , Element.height <| Element.px 150
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
                            , Element.el
                                [ Font.color Color.white
                                , Element.centerX
                                ]
                              <|
                                Element.text cardString
                            ]


viewPlayingInfo : Model -> Element.Element Msg
viewPlayingInfo model =
    let
        content =
            if
                validHand model.selected
                    && List.length (uniqueRows model.selected model.board)
                    > 1
            then
                viewPlayingInfoHand model
            else if
                anyStraight model.selected
                    && List.length model.selected
                    == 4
            then
                Element.text "3 or 5 Card Straights only"
            else if
                List.length (suitCounts model.selected)
                    == 1
                    && (List.length model.selected
                            == 3
                            || List.length model.selected
                            == 4
                       )
            then
                Element.text "Flushes must be 5 cards"
            else if rankCounts model.selected == [ 2, 2 ] then
                Element.text "Two Pair isn't a hand in Sage :("
            else
                Input.button
                    [ Element.centerX ]
                    { onPress = Just ShowHandList
                    , label = Element.text "Show Hand List"
                    }
    in
        Element.el viewWhiteBarAtts content


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
    else if
        validHand model.selected
            && List.length (uniqueRows model.selected model.board)
            > 1
    then
        viewPlayingActionCashIn model
    else
        Element.empty


viewPlayingActionTrash : Model -> Element.Element Msg
viewPlayingActionTrash model =
    let
        card =
            List.head model.selected |> Maybe.withDefault dummyCard
    in
        Input.button viewWhiteBarAtts
            { onPress = Just (Trash card)
            , label = Element.text "Trash"
            }


viewPlayingActionCashIn : Model -> Element.Element Msg
viewPlayingActionCashIn model =
    Input.button viewWhiteBarAtts
        { onPress = Just (SubmitHand model.selected)
        , label = Element.text "Cash In"
        }


viewBoard : (List Card -> Element.Element Msg) -> Board -> Element.Element Msg
viewBoard stackView board =
    let
        viewRow : Int -> Element.Element Msg
        viewRow y =
            ListX.getAt y board
                |> Maybe.withDefault [ [ dummyCard ] ]
                |> List.map stackView
                |> Element.row
                    [ Element.paddingXY 20 0
                    ]
    in
        List.range 0 (List.length board - 1)
            |> List.map viewRow
            |> Element.column [ Element.moveUp 15 ]


viewPlayingSidebar : Model -> Element.Element Msg
viewPlayingSidebar model =
    Element.column
        [ Background.color Color.lightGreen
        , Element.spaceEvenly
        , Element.paddingXY 10 20
        ]
        [ Element.el [ Element.centerX ] <|
            Element.text <|
                "Score: "
                    ++ toString model.score
        , Input.button viewWhiteBarAtts
            { onPress = Just Hint
            , label = Element.text "Hint"
            }
        , viewCard Sidebar model.selected model.board model.bonus.suit model.bonus
        , Element.el [ Element.centerX ] <|
            Element.text <|
                "Trashes: "
                    ++ toString model.trashes
        , Input.button
            (viewWhiteBarAtts
                ++ [ Element.transparent <| List.length model.selected == 0 ]
            )
            { onPress = Just Clear, label = Element.text "Clear" }
        ]


viewGameOver : Model -> Element.Element Msg
viewGameOver model =
    Element.column []
        [ Element.el [ Element.centerX, Element.centerY ] <|
            Element.text <|
                "Game over - you scored "
                    ++ toString model.score
                    ++ ". Thanks for playing."
        , viewNewGameButton
        ]


viewHandList : Element.Element Msg
viewHandList =
    let
        handListEntries =
            List.map
                (\hand -> ( hand.handName, toString hand.baseScore ++ " pts" ))
                validGameHands
    in
        Element.column []
            [ Element.el [ Element.centerX, Element.centerY ] <|
                Element.column [ Element.width Element.fill ] <|
                    List.map
                        viewHandListEntry
                        handListEntries
            , Input.button viewWhiteBarAtts
                { onPress = Just ResumePlaying
                , label = Element.text "Back to game"
                }
            ]


viewHandListEntry : ( String, String ) -> Element.Element Msg
viewHandListEntry ( hand, score ) =
    Element.row
        [ Element.padding 5
        , Element.spacing 30
        ]
        [ Element.el [ Element.alignLeft ] <| Element.text hand
        , Element.el [ Element.alignRight ] <| Element.text score
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
