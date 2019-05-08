-- module Main exposing (Model, Msg(..), hint, init, kommuneSet, kommuner, main, renderList, subscriptions, update, view)


module Main exposing (Kommune, Model, Msg(..), createHint, createResponse, getKommuner, init, kommuneDecoder, kommunerDecoder, main, renderList, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Set



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Kommune =
    { name : String
    , code : String
    }


type alias Model =
    { guessInputContent : String
    , correct : List Kommune
    , kommuner : List Kommune
    }

type NonEmptyList a = NonEmpty a (List a)

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [] []
    , getKommuner
    )



-- UPDATE


type Msg
    = Guess String
    | GotKommuner (Result Http.Error (List Kommune))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess inputGuess ->
            let
                ( newGuess, newCorrect ) =
                    case (evaluate model.kommuner model.correct inputGuess) of
                        Just kommune ->
                            ( "", kommune :: model.correct )

                        Nothing ->
                            ( inputGuess, model.correct )
            in
            ( { model | guessInputContent = newGuess, correct = newCorrect }, Cmd.none )

        GotKommuner response ->
            case response of
                Err error ->
                    Debug.todo (Debug.toString error)

                Ok newKommuner ->
                    ( { model | kommuner = newKommuner }, Cmd.none )


toLower: {a | name : String} -> {a | name : String}
toLower r =
    {r | name = String.toLower r.name}

listToLower: List {a | name : String} -> List {a | name : String}
listToLower r =
    List.map toLower r

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ Html.text ("Korrekt: " ++ String.fromInt (List.length model.correct) ++ " / " ++ String.fromInt (List.length model.kommuner)) ]
        , span []
            [ input [ value model.guessInputContent, type_ "text", placeholder "Tast inn en kommune", onInput Guess ] []
            , span [] [ Html.text (createResponse model.kommuner model.correct model.guessInputContent) ]
            ]
        , div [] [ renderList model.correct ]
        ]


renderList lst =
    lst
        |> List.map (\l -> Html.li [] [ Html.text l.name ])
        |> Html.ul []


type GuessEvaluation =
    Correct Kommune | Empty | Unknown | Partial (NonEmptyList Kommune) | AlreadyGuessed

evaluate : List Kommune -> List Kommune -> String -> GuessEvaluation
evaluate answers correct guessParameter =
    case input of
            "" ->
                Empty

            _ ->
                let
                    guess =
                        String.toLower guessParameter

                    a =
                        listToLower answers

                    c =
                        listToLower correct

                    partialMatches =
                        a
                            |> List.filter
                                (\n -> String.startsWith guess n.name)
                in
                case partialMatches of
                    [] ->
                        Unknown
                    [_] ->
                        let
                            unanswered = List.filter (\n -> List.member n c) partialMatches
                        in
                            case unanswered of
                                [] ->
                                    AlreadyGuessed
                                first::rest ->
                                    let
                                        correctAnswers = List.filter (\n -> n.name == input) partialMatches
                                    in
                                        case correctAnswers of
                                            [] -> Partial first rest
                                            [b] -> Kommune b



-- We just give hint on the first matched string in the list


createResponse : GuessEvaluation -> String
createResponse eval =
    case input of
        Empty ->
            ""
        Partial p ->
            p.name

        _ ->
            let
                i =
                    String.toLower input

                k =
                    List.map (\x -> String.toLower x.name) kommuner

                c =
                    List.map (\x -> String.toLower x.name) correct

                partialMatch =
                    k
                        |> List.filter
                            (String.startsWith i)
                        |> List.head
            in
            case partialMatch of
                Nothing ->
                    "Unknown"

                Just s ->
                    if s == i then
                        ""

                    else if List.member s c then
                        "You already guessed this!"

                    else
                        createHint s input


createHint : String -> String -> String
createHint partialMatch input =
        String.length partialMatch - String.length input
            |> (\m ->
                    String.repeat m "?"
                        |> (\o -> input ++ o)
               )



-- HTTP


getKommuner : Cmd Msg
getKommuner =
    Http.get
        { url = "kommuner.json"
        , expect = Http.expectJson GotKommuner kommunerDecoder
        }


kommuneDecoder : Decoder Kommune
kommuneDecoder =
    map2 Kommune
        (field "targetName" string)
        (field "targetCode" string)


kommunerDecoder : Decoder (List Kommune)
kommunerDecoder =
    list kommuneDecoder
