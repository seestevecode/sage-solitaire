module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList, style)
import List.Extra as LExt
import Matrix exposing (Matrix)
import Array exposing (toList)


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
    { board : Matrix Stack -- TODO: make this a Matrix
    , bonus : Suit
    , score : Int
    , trashes : Int
    , selected : List Card
    }


type alias Stack =
    List Card


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


dummyRow : List Stack
dummyRow =
    [ [ dummyCard ] ]


dummyMatrix : Matrix Stack
dummyMatrix =
    Matrix.repeat 3 3 [ dummyCard ]


type Msg
    = NoOp


initModel : Model
initModel =
    { board =
        LExt.init standardDeck
            |> Maybe.withDefault (List.repeat 51 dummyCard)
            |> LExt.groupsOfVarying [ 8, 8, 8, 7, 6, 5, 4, 3, 2 ]
            |> LExt.groupsOf 3
            |> Matrix.fromList
            |> Maybe.withDefault dummyMatrix
    , bonus =
        LExt.last standardDeck
            |> Maybe.withDefault dummyCard
            |> .suit
    , score = 0
    , trashes = 2
    , selected = []
    }


standardDeck : List Card
standardDeck =
    LExt.lift2 (flip Card)
        [ Hearts, Clubs, Diamonds, Spades ]
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
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ renderBoard model.board ]
        , Html.hr [] []
        , Html.div [] [ Html.text <| toString model.bonus ]
        , Html.div [] [ Html.text <| "Score: " ++ toString model.score ]
        , Html.div [] [ Html.text <| "Trashes: " ++ toString model.trashes ]
        , Html.hr [] []
        , Html.div [] [ Html.text <| toString model ]
        ]


renderBoard : Matrix Stack -> Html Msg
renderBoard board =
    let
        renderRow : Int -> Html Msg
        renderRow y =
            Matrix.getRow y board
                |> Maybe.map (Array.toList)
                |> Maybe.withDefault dummyRow
                |> List.map renderStack
                |> Html.div []
    in
        List.range 0 (Tuple.first board.size - 1)
            |> List.map renderRow
            |> Html.div []


renderStack : Stack -> Html Msg
renderStack cards =
    Html.div
        [ class "stack"
        , style
            [ ( "padding", "10px" )
            , ( "border", "1px solid #060" )
            , ( "display", "table-cell" )
            , ( "width", "120px" )
            , ( "height", "120px" )
            , ( "background-color", "#0c0" )
            ]
        ]
        [ cards
            |> List.head
            |> Maybe.withDefault dummyCard
            |> renderCard
        , Html.div
            [ class "stack-size"
            ]
            [ "("
                ++ toString (List.length cards)
                ++ ")"
                |> Html.text
            ]
        ]


renderCard : Card -> Html Msg
renderCard { rank, suit } =
    Html.div
        [ class "card"
        , style
            [ ( "width", "45px" )
            , ( "height", "70px" )
            , ( "border-radius", "5px" )
            , ( "border", "1px solid #666" )
            , ( "background-color", "white" )
            , ( "padding", "5px" )
            ]
        ]
        [ rankToHtml rank
        , suitToHtml suit
        ]


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
                [ ( "font", "15px Arial, sans-serif" )
                , ( "font-weight", "bold" )
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
                , ( "font-size", "66px" )
                ]
            ]
            [ Html.text s ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
