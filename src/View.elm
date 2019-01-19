module View exposing (view, viewStuff, viewWelcome)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Random exposing (Seed, int, maxInt, minInt, step)
import String exposing (fromInt)
import Time exposing (posixToMillis)


boxHeight =
    100


boxWidth =
    200


margin =
    50


shimming =
    40


px : Int -> String
px x =
    fromInt x ++ "px"


styleBox x y =
    [ style "position" "absolute"
    , style "top" (px y)
    , style "left" (px x)
    , style "border-radius" (px shimming)
    , style "width" (px boxWidth)
    , style "height" (px boxHeight)
    , style "line-height" (px (boxHeight - 30))
    , style "border" (px 3 ++ " solid")
    , style "border-color" "coral"
    ]


styleCornerBox x y =
    styleBox x y
        ++ [ style "background-color" "MediumTurquoise"
           ]


styleCenterBox x y =
    styleBox x y
        ++ [ style "background-color" "Khaki"
           ]


styleGameArea x y =
    [ style "position" "absolute"
    , style "font-size" "100%"
    , style "top" (px y)
    , style "left" (px x)
    , style "text-align" "center"
    , style "width" (px (boxWidth * 2 + shimming))
    , style "height" (px (boxHeight * 2 + shimming))
    , style "display" "block"
    , style "border-style" "solid"
    ]


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


viewStuff : Model -> Html Msg
viewStuff model =
    div
        (styleGameArea margin margin)
        [ div (styleCornerBox 0 0)
            [ p []
                [ text <| "NW = " ++ model.question.nwChoice.name
                ]
            ]
        , div (styleCornerBox (boxWidth + shimming) 0)
            [ p []
                [ text <| "ne = " ++ model.question.neChoice.name
                ]
            ]
        , div (styleCornerBox 0 (boxHeight + shimming))
            [ p []
                [ text <| "sw = " ++ model.question.swChoice.name
                ]
            ]
        , div (styleCornerBox (boxWidth + shimming) (boxHeight + shimming))
            [ p []
                [ text <| "se = " ++ model.question.seChoice.name
                ]
            ]
        , div
            (styleCenterBox ((boxWidth + shimming) // 2) ((boxHeight + shimming) // 2))
            [ p []
                [ text <| "cc = " ++ model.question.centerChoice.name
                ]
            ]
        , button
            (styleBox 0 ((boxHeight + shimming) * 2) ++ [ onClick CloseWelcomeScreen ])
            [ text "Gimme another!" ]
        ]


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome to Sort of Trivia" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Let's play!" ]
        ]
