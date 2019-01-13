module QuestionTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (all, any, foldl, length, map, range)
import List.Extra exposing (unique)
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


randomlyExcludeElements : Int -> List a -> List Bool
randomlyExcludeElements seed list =
    let
        bools =
            List.range seed (length list + seed)
                |> map (\s -> initialSeed s)
                |> map (\s -> first (step bool s))
    in
    bools


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
                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) [ USCapitals ]
                in
                Expect.false "is empty" <| isEmptyQuestion q
        , fuzz int "createQuestion returns 5 unique choices " <|
            \randSeed ->
                let
                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) [ USCapitals ]
                in
                Expect.equal 5 <| length <| toList <| choiceNames q
        , fuzz int "createQuestion returns 5 choices in the same category " <|
            \randSeed ->
                let
                    ( seed, q ) =
                        createQuestion (Random.initialSeed randSeed) [ USCapitals ]
                in
                Expect.equal 5 <| length <| toList <| choiceNames q
        , todo "Need to test that it only returns correct categories"
        ]
