module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (id, class, classList, style)
import Html.Events exposing (onClick)
import List.Extra as ListX
import Random exposing (Generator)
import Random.List
import Maybe.Extra as MaybeX


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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                            model.score + (hand.baseScore * 2)
                        else
                            model.score + hand.baseScore
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
            ( { model | gameState = Playing }, Cmd.none )

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
    Html.button [ onClick StartGame ] [ Html.text "Start new game" ]


viewHandList : Html Msg
viewHandList =
    Html.div []
        [ Html.text "Hand list goes here"
        , Html.button [ onClick ResumePlaying ] [ Html.text "Back to game" ]
        ]


viewPlaying : Model -> Html Msg
viewPlaying model =
    let
        cardView =
            viewCard model.selected model.board model.bonus.suit

        stackView =
            viewStack cardView
    in
        Html.div []
            [ Html.div [ id "board" ] [ viewBoard stackView model.board ]
            , viewPlayingInfo model
            , viewPlayingActions model
            , viewPlayingSidebar model
            , viewPlayingDebugging model
            ]


viewPlayingInfo : Model -> Html Msg
viewPlayingInfo model =
    if
        validHand model.selected
            && List.length (uniqueRows model.selected model.board)
            > 1
    then
        viewPlayingInfoHand model
    else if anyStraight model.selected && List.length model.selected == 4 then
        Html.text "3 or 5 Card Straights only"
    else if
        List.length (suitCounts model.selected)
            == 1
            && (List.length model.selected == 3 || List.length model.selected == 4)
    then
        Html.text "Flushes must be 5 cards"
    else if rankCounts model.selected == [ 2, 2 ] then
        Html.text "Two Pair isn't a hand in Sage :("
    else
        Html.button [ onClick ShowHandList ] [ Html.text "Tap for hand list" ]


viewGameOver : Html Msg
viewGameOver =
    Html.text "Game over - thanks for playing"


viewPlayingInfoHand : Model -> Html Msg
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
        Html.text <|
            hand.handName
                ++ " ("
                ++ toString hand.baseScore
                ++ " pts"
                ++ bonusText
                ++ ")"


viewPlayingActions : Model -> Html Msg
viewPlayingActions model =
    if List.length model.selected == 1 && model.trashes > 0 then
        viewPlayingActionTrash model
    else if validHand model.selected && List.length (uniqueRows model.selected model.board) > 1 then
        viewPlayingActionCashIn model
    else
        Html.text ""


viewPlayingActionTrash : Model -> Html Msg
viewPlayingActionTrash model =
    let
        card =
            List.head model.selected |> Maybe.withDefault dummyCard
    in
        Html.button [ onClick (Trash card) ] [ Html.text "Trash" ]


viewPlayingActionCashIn : Model -> Html Msg
viewPlayingActionCashIn model =
    Html.button [ onClick (SubmitHand model.selected) ] [ Html.text "Cash In" ]


viewPlayingSidebar : Model -> Html Msg
viewPlayingSidebar model =
    Html.div []
        [ Html.p [ id "score" ] [ Html.text <| "Score: " ++ toString model.score ]
        , Html.button [ id "hint", onClick Hint ] [ Html.text "Hint" ]
        , Html.div [ id "bonus" ] [ Html.text <| "Bonus: " ++ toString model.bonus ]
        , Html.p [ id "trashes" ] [ Html.text <| "Trashes: " ++ toString model.trashes ]
        , Html.p [ id "discarded" ] [ Html.text <| "Discarded: " ++ toString (List.length model.discarded) ]
        , if List.length model.selected > 0 then
            Html.button [ onClick Clear ] [ Html.text "Clear" ]
          else
            Html.text ""
        ]


viewPlayingDebugging : Model -> Html Msg
viewPlayingDebugging model =
    let
        scoredHands =
            scoredHandsFromBoard model.board model.bonus.suit
    in
        Html.div []
            [ Html.p [] [ Html.text <| "Possible scores: " ++ toString (List.map .actualScore scoredHands) ]
            , Html.p [] [ Html.text <| "Best score: " ++ toString (.actualScore <| bestHandFromScored scoredHands) ]
            , Html.p [] [ Html.text <| "Discarded: " ++ toString model.discarded ]
            , Html.p [] [ Html.text <| "Game state: " ++ toString model.gameState ]
            , Html.p [] [ Html.text <| "No more moves: " ++ toString (noMoreMoves model) ]
            , Html.p [] [ Html.text <| "Suits selected: " ++ toString (suitCounts model.selected) ]
            , Html.p [] [ Html.text <| "Ranks selected: " ++ toString (rankCounts model.selected) ]
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


viewBoard : (List Card -> Html Msg) -> List (List (List Card)) -> Html Msg
viewBoard stackView board =
    let
        viewRow : Int -> Html Msg
        viewRow y =
            ListX.getAt y board
                |> Maybe.withDefault dummyRow
                |> List.map stackView
                |> Html.div []
    in
        List.range 0 (List.length board - 1)
            |> List.map viewRow
            |> Html.div []


viewStack : (Card -> Html Msg) -> List Card -> Html Msg
viewStack cardView cards =
    let
        stackAtts =
            [ class "stack"
            , style
                [ ( "display", "table-cell" )
                , ( "width", "100px" )
                , ( "height", "100px" )
                , ( "border", "1px solid #0c4" )
                , ( "background-color", "#093" )
                , ( "padding", "15px 25px" )
                ]
            ]
    in
        case List.head cards of
            Just card ->
                Html.div stackAtts
                    [ cardView card
                    , Html.text "🂠"
                        |> List.repeat (List.length cards - 1)
                        |> Html.div
                            [ class "stack-size"
                            , style
                                [ ( "margin", "auto" )
                                , ( "padding-top", "10px" )
                                , ( "color", "white" )
                                , ( "text-align", "center" )
                                ]
                            ]
                    ]

            Nothing ->
                Html.div stackAtts [ Html.text "" ]


viewCard : List Card -> List (List (List Card)) -> Suit -> Card -> Html Msg
viewCard selected board bonus card =
    let
        selectionColor =
            if validHand selected && List.length (uniqueRows selected board) > 1 then
                "gold"
            else
                "red"

        selectionStyle =
            ( "box-shadow"
            , "3px 3px 1px "
                ++ selectionColor
                ++ ", -3px -3px 1px "
                ++ selectionColor
                ++ ", 3px -3px 1px "
                ++ selectionColor
                ++ ", -3px 3px 1px "
                ++ selectionColor
            )

        defaultCardStyles =
            [ ( "width", "50px" )
            , ( "height", "70px" )
            , ( "background-color", "white" )
            , ( "margin", "auto" )
            , ( "border", "1px solid #999" )
            , ( "border-radius", "5px" )
            , ( "position", "relative" )
            , ( "cursor", "pointer" )
            ]

        cardStyles =
            if List.member card selected then
                selectionStyle :: defaultCardStyles
            else
                defaultCardStyles
    in
        Html.div
            [ class "card"
            , style cardStyles
            , onClick (ToggleCard card)
            ]
            [ rankToHtml card.rank
            , suitToHtml card.suit
            , bonusStar bonus card.suit
            ]


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


rankToHtml : Rank -> Html Msg
rankToHtml rank =
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
        Html.div
            [ classList
                [ ( "rank", True )
                , ( String.toLower <| toString rank, True )
                ]
            , style
                [ ( "font", "35px Arial,sans-serif" )
                , ( "font-weight", "bold" )
                , ( "position", "absolute" )
                , ( "bottom", "0" )
                , ( "right", "5px" )
                ]
            ]
            [ Html.text r ]


suitToHtml : Suit -> Html Msg
suitToHtml suit =
    let
        ( symbol, colour ) =
            case suit of
                Hearts ->
                    ( "♥", "red" )

                Clubs ->
                    ( "♣", "black" )

                Diamonds ->
                    ( "♦", "red" )

                Spades ->
                    ( "♠", "black" )
    in
        Html.div
            [ classList
                [ ( "suit", True )
                , ( String.toLower <| toString suit, True )
                ]
            , style
                [ ( "color", colour )
                , ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "5px" )
                , ( "font-size", "30px" )
                ]
            ]
            [ Html.text symbol ]


bonusStar : Suit -> Suit -> Html Msg
bonusStar bonusSuit cardSuit =
    let
        bonusHtml =
            if cardSuit == bonusSuit then
                Html.span
                    [ class "star"
                    , style
                        [ ( "position", "absolute" )
                        , ( "top", "2px" )
                        , ( "right", "5px" )
                        , ( "color", "orange" )
                        ]
                    ]
                    [ Html.text "★" ]
            else
                Html.text ""
    in
        Html.div
            [ class "bonus" ]
            [ bonusHtml ]


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
