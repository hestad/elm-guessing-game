-- module Main exposing (Model, Msg(..), hint, init, kommuneSet, kommuner, main, renderList, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set
import Http
import Json.Decode exposing (Decoder, field, int, string, list, map2)


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
    { name: String,
      code: String
    }

type alias Model =
    { guessInputContent : String
    , correct : List Kommune
    , kommuner: List Kommune
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [] []
    , getKommuner
    )



-- UPDATE


type Msg
    = Guess String |
      GotKommuner (Result Http.Error (List Kommune))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess inputGuess ->
            let
                ( newGuess, newCorrect ) =
                    case tryGetKommune model.kommuner inputGuess of
                            Just kommune -> ( "", kommune :: model.correct )
                            Nothing -> ( inputGuess, model.correct )
            in
            ( { model | guessInputContent = newGuess, correct = newCorrect }, Cmd.none )
        GotKommuner response ->
            case response of
                Err error -> Debug.todo (Debug.toString error)
                Ok newKommuner ->  ( { model | kommuner=newKommuner}, Cmd.none )




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
            , span [] [ Html.text (hint model.kommuner model.guessInputContent) ]
            ]
        , div [] [ renderList model.correct ]
        ]



renderList lst =
    lst
        |> List.map (\l -> Html.li [] [ Html.text l.name ])
        |> Html.ul []


tryGetKommune : (List Kommune) -> String -> Maybe Kommune
tryGetKommune kommuner input =
    kommuner
        |> List.filter
            (\n -> String.contains (String.toLower n.name) (String.toLower input))
        |> List.head


-- TODO: What if input matches multiple communes


hint : (List Kommune) -> String -> String
hint kommuner input =
    case input of
        "" ->
            ""

        _ ->
            kommuner
                |> List.map
                    (\n -> n.name)
                |> List.filter
                    (\n -> String.startsWith (String.toLower input) (String.toLower n))
                |> List.head
                |> Maybe.map
                    String.length
                |> Maybe.map
                    (\n -> n - String.length input)
                |> Maybe.map
                    (\n -> String.repeat n "?")
                |> Maybe.map
                    (\n -> input ++ n)
                |> Maybe.withDefault
                    "Ukjent"

-- HTTP


getKommuner : Cmd Msg
getKommuner =
    Http.get
    {url = "kommuner.json"
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
