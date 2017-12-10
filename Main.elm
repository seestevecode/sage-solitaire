module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList)


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
    [ Card Ace Spades
    , Card Queen Hearts
    , Card Nine Clubs
    , Card Three Diamonds
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "card-list" ] <|
            List.map renderCard model
        , Html.br [] []
        , Html.text <| toString model
        ]


renderCard : Card -> Html Msg
renderCard { rank, suit } =
    Html.div [ class "card" ]
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
