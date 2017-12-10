module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList, style)
import List.Extra as LExt


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
    List Stack


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


type Msg
    = NoOp


initModel : Model
initModel =
    LExt.groupsOfVarying
        [ 8, 8, 8, 7, 6, 5, 4, 3, 2, 1 ]
        standardDeck


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
        [ Html.div [] <| List.map renderStack model
        , Html.hr [] []
        , Html.div [] [ Html.text <| toString model ]
        ]


renderStack : Stack -> Html Msg
renderStack cards =
    Html.div
        [ class "stack"
        , style
            [ ( "padding", "10px" )
            , ( "border", "1px solid #060" )
            , ( "display", "table-cell" )
            , ( "width", "70px" )
            , ( "height", "70px" )
            , ( "margin", "0 auto" )
            , ( "position", "relative" )
            , ( "background-color", "#0c0" )
            ]
        ]
        [ cards
            |> List.head
            |> Maybe.withDefault (Card Ace Spades)
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
            [ ( "width", "30px" )
            , ( "height", "40px" )
            , ( "border-radius", "4px" )
            , ( "margin", "5px" )
            , ( "border", "1px solid grey" )
            , ( "background-color", "white" )
            , ( "font-size", "18px" )
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
        Html.span
            [ classList
                [ ( "rank", True )
                , ( String.toLower <| toString rank, True )
                ]
            ]
            [ Html.text r ]


suitToHtml : Suit -> Html Msg
suitToHtml suit =
    let
        s =
            case suit of
                Hearts ->
                    "♥"

                Clubs ->
                    "♣"

                Diamonds ->
                    "♦"

                Spades ->
                    "♠"
    in
        Html.span
            [ classList
                [ ( "suit", True )
                , ( String.toLower <| toString suit, True )
                ]
            ]
            [ Html.text s ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
