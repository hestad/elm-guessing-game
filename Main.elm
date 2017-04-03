--import Html

--main = Html.text (String.join ", " kommuner)

import Html exposing (Html, Attribute, div, input, text, span, li, ul, node)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Set

kommuner : List String
kommuner = ["skien","porsgrunn","notodden","bamble","kragerø","nome","bø","tinn","sauherad","drangedal","vinje","seljord","kviteseid","siljan","tokke","hjartdal","nissedal","fyresdal"]

kommuneSet : Set.Set String
kommuneSet = Set.fromList kommuner

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { content : String,
    correct : List String
  }

model : Model
model =
  { content = "",
   correct = []}


-- UPDATE

type Msg
  = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      let (c, newCorrect) = if (Set.member newContent kommuneSet) then ("", newContent :: model.correct) else (newContent, model.correct)
      in { model | content = c, correct = newCorrect }



-- VIEW

view : Model -> Html Msg
view model =
    div []
    [
        div [] [Html.text ("Korrekt: " ++ toString (List.length model.correct) ++ " / " ++ toString (Set.size kommuneSet))],
        span []
            [ input [ value model.content ,type_ "text", placeholder "Tast inn en kommune", onInput Change ] []
            , span [] [ Html.text (hint model.content) ]
            ],
        div [] [renderList model.correct]
    ]

--renderList : List String -> Html Msg
renderList lst =
    lst
       |> List.map (\l -> Html.li [] [ Html.text l ])
       |> Html.ul []

-- TODO: What if input matches multiple communes
hint : String -> String
hint input = case input of
                "" ->
                    ""

                _ ->
                    kommuner

                       |> List.filter
                            (\n -> String.startsWith (String.toLower input) (String.toLower n) )

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