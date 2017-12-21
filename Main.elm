module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import List.Extra as ListX
import Random exposing (Generator)
import Random.List
import Set exposing (Set)


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
    }


type alias Card =
    { rank : Rank, suit : Suit }


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


dummyBoard : List (List (List Card))
dummyBoard =
    List.repeat 3 dummyRow


type Msg
    = OnShuffle (List Card)
    | ToggleCard Card
    | Trash Card


initModel : Model
initModel =
    { board = boardFromDeck standardDeck
    , bonus = bonusFromDeck standardDeck
    , score = 0
    , trashes = 2
    , selected = []
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
              }
            , Cmd.none
            )


shuffledCardsGenerator : Generator (List Card)
shuffledCardsGenerator =
    Random.List.shuffle standardDeck



-- VIEW


view : Model -> Html Msg
view model =
    let
        cardView =
            viewCard model.selected model.board model.bonus.suit

        stackView =
            viewStack cardView
    in
        Html.div []
            [ Html.div [] [ viewBoard stackView model.board ]
            , Html.div [] [ viewActions model.selected model.trashes ]
            , Html.hr [] []
            , Html.div [] [ Html.text <| "Bonus: " ++ toString model.bonus ]
            , Html.div [] [ Html.text <| "Score: " ++ toString model.score ]
            , Html.div [] [ Html.text <| "Trashes: " ++ toString model.trashes ]
            , Html.hr [] []
            , Html.div []
                [ Html.p [] [ Html.text <| "Top cards: " ++ toString model.board ]
                , Html.p [] [ Html.text <| "Selected: " ++ toString model.selected ]
                , Html.p [] [ Html.text <| "Rows sel: " ++ toString (uniqueRows model.selected model.board) ]
                ]
            ]


viewActions : List Card -> Int -> Html Msg
viewActions selected trashes =
    if List.length selected == 1 && trashes > 0 then
        let
            card =
                List.head selected |> Maybe.withDefault dummyCard
        in
            Html.button [ onClick (Trash card) ] [ Html.text "Trash" ]
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


uniqueRows : List Card -> List (List (List Card)) -> Int
uniqueRows searches grid =
    searches
        |> List.concatMap (inRowAll grid)
        |> Set.fromList
        |> Set.size


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
viewCard selected visible bonus card =
    let
        selectionColor =
            if (validHand selected) && (uniqueRows selected visible > 1) then
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
        ( s, clr ) =
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
                [ ( "color", clr )
                , ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "5px" )
                , ( "font-size", "30px" )
                ]
            ]
            [ Html.text s ]


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
subscriptions model =
    Sub.none
