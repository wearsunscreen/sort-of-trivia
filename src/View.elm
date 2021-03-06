module View exposing (view, viewStuff, viewWelcome)

import Browser exposing (Document)
import DnD exposing (Draggable, MousePosition)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (append, filter, head, indexedMap, member)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Model exposing (..)
import Question exposing (Choice, Direction(..), getChoiceNames, getCorrectAt)
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


boxStyles : ( Int, Int ) -> List (Html.Attribute Msg)
boxStyles ( x, y ) =
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


buttonView : Int -> ( String, Msg ) -> Html Msg
buttonView index ( label, msg ) =
    button
        (buttonStyles
            ( buttonX index, buttonY index )
            ++ [ onClick msg ]
        )
        [ text label ]


buttons : Model -> Html Msg
buttons model =
    div []
        (List.indexedMap
            buttonView
            [ ( "Test!", TestAnswers )
            , ( "Reset", ResetQuestion )
            , ( "Gimme another!", NextQuestion )
            ]
        )


buttonStyles : ( Int, Int ) -> List (Html.Attribute Msg)
buttonStyles ( x, y ) =
    [ style "position" "absolute"
    , style "top" (px y)
    , style "left" (px x)
    , style "font-size" "150%"
    , style "border-radius" (px shimming)
    , style "height" (px (boxHeight // 3))
    , style "line-height" (px (boxHeight // 4))
    , style "border" (px 3 ++ " solid")
    , style "border-color" "darkslateblue"
    , style "background-color" "lavender"
    ]


buttonX : Int -> Int
buttonX iBtn =
    margin


buttonY : Int -> Int
buttonY iButton =
    (boxHeight + shimming) * 2 + margin + ((boxHeight // 2) * iButton)


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


gameAreaStyles : ( Int, Int ) -> List (Html.Attribute Msg)
gameAreaStyles ( x, y ) =
    [ style "position" "absolute"
    , style "font-size" "120%"
    , style "top" (px y)
    , style "left" (px x)
    , style "text-align" "center"
    , style "width" (px (boxWidth * 2 + shimming))
    , style "height" (px (boxHeight * 2 + shimming))
    , style "display" "block"
    ]


{-| Get the name of the choice that was dropped into specified pot
-}
getPotNameAt : Mode -> Direction -> List Choice -> String
getPotNameAt mode dir list =
    let
        label =
            case dir of
                CC ->
                    "Centrally located city"

                -- error, this should never be seen
                Unused ->
                    "I'm so confused!"

                NE ->
                    "Northeastern most city"

                NW ->
                    "Northwestern most city"

                SE ->
                    "Southeastern most city"

                SW ->
                    "Southwestern most city"

        choice =
            List.Extra.dropWhile (\c -> c.potDirection /= dir) list |> head
    in
    case choice of
        Nothing ->
            if mode == Test then
                "Try Again!"

            else
                label

        Just c ->
            c.name


{-| Draw a pot. A pot is a droppable. Choices are dragged and dropped into pots.
-}
potBox : Model -> Direction -> Html Msg
potBox model dir =
    let
        index =
            toIndex dir

        -- Relative positions of pot droppables
        potOffset : ( Int, Int )
        potOffset =
            let
                offsets =
                    [ ( 0, 0 )
                    , ( boxWidth + shimming, 0 )
                    , ( boxWidth + shimming, boxHeight + shimming )
                    , ( 0, boxHeight + shimming )
                    , ( (boxWidth + shimming) // 2, (boxHeight + shimming) // 2 )
                    ]
            in
            getAt index offsets |> withDefault ( 0, 0 )

        potColor : Direction -> String
        potColor i =
            let
                c =
                    getCorrectAt i model.options
            in
            if model.mode == Test then
                if c.correctDirection == c.potDirection then
                    "LawnGreen"

                else
                    "Tomato"

            else
                case i of
                    CC ->
                        "Khaki"

                    _ ->
                        "MediumTurquoise"
    in
    dnd.droppable dir
        (potStyles potOffset (potColor dir))
        [ p []
            [ text <| getPotNameAt model.mode dir model.options
            ]
        ]


potStyles : ( Int, Int ) -> String -> List (Html.Attribute Msg)
potStyles ( x, y ) color =
    boxStyles ( x, y )
        ++ [ style "background-color" color
           ]


px : Int -> String
px x =
    fromInt x ++ "px"


toIndex : Direction -> Int
toIndex dir =
    case dir of
        CC ->
            4

        NE ->
            1

        NW ->
            0

        SE ->
            2

        SW ->
            3

        Unused ->
            -1


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
            -- draw the pots
            (gameAreaStyles ( margin, margin ))
            (List.map
                (potBox model)
                [ NW, NE, SE, SW, CC ]
            )
         , buttons model
         , DnD.dragged
            model.draggable
            dragBox
         ]
            ++ List.indexedMap
                (\index choice -> dnd.draggable choice [] [ choiceBox index choice ])
                (filter (\choice -> choice.potDirection == Unused) model.options)
        )


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ p [] [ h1 [] [ text "Welcome to Sort of Trivia" ] ]
        , button [ onClick CloseWelcomeScreen ] [ text "Let's play!" ]
        ]
