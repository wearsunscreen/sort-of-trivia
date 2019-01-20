module View exposing (view, viewStuff, viewWelcome)

import Browser exposing (Document)
import DnD exposing (Draggable, MousePosition)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (append, indexedMap)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Model exposing (..)
import Question exposing (Choice, getChoiceNameAt, getChoiceNames, iCC, iNE, iNW, iSE, iSW)
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


choiceBox : Int -> Choice -> Html Msg
choiceBox index choice =
    div (choiceStyles ( (boxWidth + shimming) * 2 + margin, margin + (index * (boxHeight // 2)) ))
        [ text choice.name ]


dragBox : Choice -> Html Msg
dragBox choice =
    div
        [ style "border-radius" (px (shimming // 2))
        , style "border" (px 3 ++ " solid")
        , style "border-color" "teal"
        , style "background-color" "lightblue"
        , style "padding" (px 8)
        ]
        [ text choice.name ]


choiceStyles : ( Int, Int ) -> List (Html.Attribute Msg)
choiceStyles ( x, y ) =
    [ style "position" "absolute"
    , style "top" (px y)
    , style "left" (px x)
    , style "border-radius" (px (shimming // 2))
    , style "border" (px 3 ++ " solid")
    , style "border-color" "teal"
    , style "background-color" "lightblue"
    , style "padding" (px 8)
    ]


{-| Draw a pot. A pot is a droppable. Choices are dragged and dropped into pots.
-}
potBox : Model -> Int -> Html Msg
potBox model index =
    dnd.droppable ()
        (potStyles (potOffset index) (potColor index))
        [ p []
            [ text <| getChoiceNameAt index model.pots
            ]
        ]


potColor : Int -> String
potColor i =
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


{-| Relative positions of pot droppables
-}
potOffset : Int -> ( Int, Int )
potOffset i =
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


potStyles : ( Int, Int ) -> String -> List (Html.Attribute Msg)
potStyles ( x, y ) color =
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
        ([ div
            (styleGameArea ( margin, margin ))
            (List.map
                (potBox model)
                [ iNW, iNE, iSE, iSW, iCC ]
            )
         , button
            (styleBox ( margin, (boxHeight + shimming) * 2 + margin ) ++ [ onClick NextQuestion ])
            [ text "Gimme another!" ]
         , DnD.dragged
            model.draggable
            dragBox
         ]
            ++ List.indexedMap
                (\index choice -> dnd.draggable choice [] [ choiceBox index choice ])
                model.question
        )


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome to Sort of Trivia" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Let's play!" ]
        ]
