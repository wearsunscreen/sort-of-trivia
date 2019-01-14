module QuestionTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (all, any, foldl, length, map, member, range)
import List.Extra exposing (unique, zip)
import Question exposing (Category(..), Question, createQuestion)
import Random exposing (Generator, initialSeed, step)
import Set exposing (Set, fromList, toList)
import Test exposing (..)
import Tuple exposing (first, second)


choiceCategories : Question -> List Category
choiceCategories q =
    let
        unique list =
            case list of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: xs ->
                    if List.member x xs then
                        unique xs

                    else
                        x :: unique xs
    in
    unique
        [ q.centerChoice.category
        , q.neChoice.category
        , q.nwChoice.category
        , q.seChoice.category
        , q.swChoice.category
        ]


choiceNames : Question -> Set String
choiceNames q =
    fromList
        [ q.centerChoice.city
        , q.neChoice.city
        , q.nwChoice.city
        , q.seChoice.city
        , q.swChoice.city
        ]


isEmptyQuestion : Question -> Bool
isEmptyQuestion q =
    any ((==) True)
        [ q.centerChoice.category == EmptyCategory
        , q.neChoice.category == EmptyCategory
        , q.nwChoice.category == EmptyCategory
        , q.seChoice.category == EmptyCategory
        , q.swChoice.category == EmptyCategory
        ]


{-| stole this from Random.elm, don't know why I couldn't import it
-}
bool : Generator Bool
bool =
    Random.map ((==) 1) (Random.int 0 1)


randomCategories : Int -> List Category
randomCategories seed =
    let
        cats =
            randomlyExcludeElements seed
                [ USCapitals
                , MXCapitals
                , WorldCapitals
                ]
    in
    case cats of
        [] ->
            [ USCapitals ]

        _ ->
            cats


randomlyExcludeElements : Int -> List a -> List a
randomlyExcludeElements seed list =
    let
        bools =
            List.range seed (length list + seed)
                |> map (\s -> initialSeed s)
                |> map (\s -> first (step bool s))
                |> zip list

        f : ( a, Bool ) -> List a -> List a
        f ( cat, yesNo ) acc =
            if yesNo then
                cat :: acc

            else
                acc
    in
    foldl f [] bools


suite : Test
suite =
    describe "Test Question.createQuestion"
        [ fuzz int "Seeds should not repeat" <|
            \randomSeed ->
                let
                    seed =
                        Random.initialSeed randomSeed
                in
                Expect.notEqual seed <| first <| createQuestion seed [ USCapitals ]
        , fuzz int "createQuestion returns a non-empty question" <|
            \randSeed ->
                let
                    cats =
                        randomCategories randSeed

                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) cats
                in
                Expect.false "is empty" <| isEmptyQuestion q
        , fuzz int "createQuestion returns 5 unique choices " <|
            \randSeed ->
                let
                    cats =
                        randomCategories randSeed

                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) cats
                in
                Expect.equal 5 <| length <| toList <| choiceNames q
        , fuzz int "createQuestion returns 5 choices in the given categories" <|
            \randSeed ->
                let
                    cats =
                        randomCategories 25

                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) cats
                in
                Expect.true "all choices are in the categories specified"
                    (all
                        ((==) True)
                        (map
                            (\c -> member c cats)
                            (choiceCategories q)
                        )
                    )
        ]
