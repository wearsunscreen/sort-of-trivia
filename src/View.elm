module View exposing (view, viewStuff, viewWelcome)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Random exposing (Seed, int, maxInt, minInt, step)
import String exposing (fromInt)
import Time exposing (posixToMillis)


view : Model -> Document Msg
view model =
    let
        b =
            case model.startTime of
                Nothing ->
                    viewWelcome model

                Just t ->
                    viewStuff model
    in
    { title = "Sort of Trivia"
    , body = [ b ]
    }


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome!" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Ok" ]
        ]


viewStuff : Model -> Html Msg
viewStuff model =
    let
        seed =
            case model.randomSeed of
                Nothing ->
                    "unknown"

                Just s ->
                    Random.step (int minInt maxInt) s
                        |> Tuple.first
                        |> fromInt

        time =
            case model.startTime of
                Nothing ->
                    "unknown"

                Just t ->
                    t |> posixToMillis |> fromInt
    in
    div []
        [ p []
            [ text <| "center = " ++ model.question.centerChoice.name
            , br [] []
            , text <| "nw = " ++ model.question.nwChoice.name
            , br [] []
            , text <| "ne = " ++ model.question.neChoice.name
            , br [] []
            , text <| "sw = " ++ model.question.swChoice.name
            , br [] []
            , text <| "se = " ++ model.question.seChoice.name
            ]
        , p []
            [ text "Start time is  "
            , text time
            ]
        , p []
            [ text "Your psuedo random number is  "
            , text seed
            ]
        ]
