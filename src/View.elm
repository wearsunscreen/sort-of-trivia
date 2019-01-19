module View exposing (view, viewStuff, viewWelcome)

import Browser exposing (Document)
import DnD exposing (Draggable)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (append)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Model exposing (..)
import Question exposing (getChoiceNameAt, iCC, iNE, iNW, iSE, iSW)
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


{-| Draw an answer droppable
-}
answerBox : Model -> Int -> Html Msg
answerBox model index =
    div (answerStyles (answerOffset index) (answerColor index))
        [ p []
            [ text <| getChoiceNameAt index model.answers
            ]
        ]


answerColor : Int -> String
answerColor i =
    let
        offsets =
            [ "MediumTurquoise"
            , "MediumTurquoise"
            , "MediumTurquoise"
            , "MediumTurquoise"
            , "Khaki"
            ]
    in
    getAt i offsets |> withDefault "MintCream"


{-| Relative positions of answer droppables
-}
answerOffset : Int -> ( Int, Int )
answerOffset i =
    let
        offsets =
            [ ( 0, 0 )
            , ( boxWidth + shimming, 0 )
            , ( boxWidth + shimming, boxHeight + shimming )
            , ( 0, boxHeight + shimming )
            , ( (boxWidth + shimming) // 2, (boxHeight + shimming) // 2 )
            ]
    in
    getAt i offsets |> withDefault ( 0, 0 )


answerStyles : ( Int, Int ) -> String -> List (Html.Attribute Msg)
answerStyles ( x, y ) color =
    styleBox ( x, y )
        ++ [ style "background-color" color
           ]


px : Int -> String
px x =
    fromInt x ++ "px"


styleBox : ( Int, Int ) -> List (Html.Attribute Msg)
styleBox ( x, y ) =
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


styleGameArea : ( Int, Int ) -> List (Html.Attribute Msg)
styleGameArea ( x, y ) =
    [ style "position" "absolute"
    , style "font-size" "100%"
    , style "top" (px y)
    , style "left" (px x)
    , style "text-align" "center"
    , style "width" (px (boxWidth * 2 + shimming))
    , style "height" (px (boxHeight * 2 + shimming))
    , style "display" "block"
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
    div []
        [ div
            (styleGameArea ( margin, margin ))
            (List.map
                (answerBox model)
                [ iNW, iNE, iSE, iSW, iCC ]
            )
        , button
            (styleBox ( margin, (boxHeight + shimming) * 2 + margin ) ++ [ onClick NextQuestion ])
            [ text "Gimme another!" ]
        ]


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome to Sort of Trivia" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Let's play!" ]
        ]
