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


initModel : Model
initModel =
    { board = boardFromDeck standardDeck
    , bonus = bonusFromDeck standardDeck
    , score = 0
    , trashes = 2
    , selected = []
    , discarded = []
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
            , Cmd.none
            )

        Clear ->
            ( { model
                | selected = []
              }
            , Cmd.none
            )

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


shuffledCardsGenerator : Generator (List Card)
shuffledCardsGenerator =
    Random.List.shuffle standardDeck


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
            else if flush cards then
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
    let
        cardView =
            viewCard model.selected model.board model.bonus.suit

        stackView =
            viewStack cardView

        scoredHands =
            scoredHandsFromBoard model.board model.bonus.suit
    in
        Html.div []
            [ Html.div [ id "board" ] [ viewBoard stackView model.board ]
            , if ListX.isPermutationOf model.selected (bestHandFromScored scoredHands |> .hand) then
                Html.text ""
              else
                Html.button [ id "hint", onClick Hint ] [ Html.text "Hint" ]
            , Html.div [ id "actions" ] [ viewActions model.selected model.bonus.suit model.board model.trashes ]
            , Html.hr [] []
            , Html.div []
                [ Html.div [ id "bonus" ] [ Html.text <| "Bonus: " ++ toString model.bonus ]
                , Html.p [ id "score" ] [ Html.text <| "Score: " ++ toString model.score ]
                , Html.p [ id "trashes" ] [ Html.text <| "Trashes: " ++ toString model.trashes ]
                , Html.p [ id "discarded" ] [ Html.text <| "Discarded: " ++ toString (List.length model.discarded) ]
                , if List.length model.selected > 0 then
                    Html.button [ onClick Clear ] [ Html.text "Clear" ]
                  else
                    Html.text ""
                ]
            , Html.hr [] []
            , Html.div []
                [ Html.p [] [ Html.text <| "Possible scores: " ++ toString (List.map .actualScore scoredHands) ]
                , Html.p [] [ Html.text <| "Best score: " ++ toString (.actualScore <| bestHandFromScored scoredHands) ]
                , Html.p [] [ Html.text <| "Discarded: " ++ toString model.discarded ]
                ]
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


viewActions : List Card -> Suit -> List (List (List Card)) -> Int -> Html Msg
viewActions selected bonus board trashes =
    if List.length selected == 1 && trashes > 0 then
        let
            card =
                List.head selected |> Maybe.withDefault dummyCard
        in
            Html.button [ onClick (Trash card) ] [ Html.text "Trash" ]
    else if validHand selected && List.length (uniqueRows selected board) > 1 then
        let
            hand =
                scoreHand selected bonus

            bonusText =
                if hand.isBonus then
                    " x2"
                else
                    ""
        in
            Html.button [ onClick (SubmitHand selected) ]
                [ hand.handName
                    ++ " ("
                    ++ toString hand.baseScore
                    ++ " pts"
                    ++ bonusText
                    ++ ")"
                    |> Html.text
                ]
    else
        Html.text ""


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
        || flush sel
        || fourOfAKind sel
        || straightFlush sel


pair : List Card -> Bool
pair sel =
    (List.length sel == 2) && (List.length (uniqueRanks sel) == 1)


threeOfAKind : List Card -> Bool
threeOfAKind sel =
    (List.length sel == 3) && (List.length (uniqueRanks sel) == 1)


fourOfAKind : List Card -> Bool
fourOfAKind sel =
    (List.length sel == 4) && (List.length (uniqueRanks sel) == 1)


flush : List Card -> Bool
flush sel =
    (List.length sel == 5) && (List.length (uniqueSuits sel) == 1)


straight : List Card -> Bool
straight sel =
    sel
        |> List.map .rank
        |> ListX.permutations
        |> List.any (flip ListX.isInfixOf loopedRanks)


threeCardStraight : List Card -> Bool
threeCardStraight sel =
    (List.length sel == 3) && straight sel


fiveCardStraight : List Card -> Bool
fiveCardStraight sel =
    (List.length sel == 5) && straight sel


straightFlush : List Card -> Bool
straightFlush sel =
    fiveCardStraight sel && flush sel


fullHouse : List Card -> Bool
fullHouse sel =
    let
        listCounts =
            sel
                |> List.map .rank
                |> List.sortBy rankToInt
                |> ListX.group
                |> List.map List.length
    in
        listCounts == [ 2, 3 ] || listCounts == [ 3, 2 ]


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


uniqueRanks : List Card -> List Rank
uniqueRanks cards =
    cards |> List.map .rank |> ListX.uniqueBy rankToInt


rankToInt : Rank -> Int
rankToInt rank =
    ListX.elemIndex rank orderedRanks |> Maybe.withDefault 1


uniqueSuits : List Card -> List Suit
uniqueSuits cards =
    cards |> List.map .suit |> ListX.uniqueBy suitToInt


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
