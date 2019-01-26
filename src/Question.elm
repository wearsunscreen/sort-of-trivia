module Question exposing
    ( Category(..)
    , Choice
    , Direction(..)
    , allCategories
    , createQuestion
    , favoredCategory
    , getChoiceNameAt
    , getChoiceNames
    )

import List exposing (drop, filter, head, length, map, member, reverse, sortBy, take)
import List.Extra exposing (getAt, unique)
import Maybe exposing (withDefault)
import Random exposing (Seed, int, step)
import String exposing (fromInt)
import Tuple exposing (second)


type Category
    = USCapitals
    | MXCapitals
    | WorldCapitals
    | EmptyCategory


type alias Choice =
    { category : Category
    , detail : String
    , measureX : Float
    , measureY : Float
    , name : String
    , potIndex : Int
    }


{-| Indices of choices NW, NE, SE, SW, CC
-}
type Direction
    = CC
    | NE
    | NW
    | SE
    | SW



{- iCC =
       4


   iNE =
       1


   iNW =
       0


   iSE =
       2


   iSW =
       3
-}


{-| used in testing
-}
allCategories : List Category
allCategories =
    [ USCapitals, MXCapitals, WorldCapitals ]


{-| create a multiple choice question by randomly selecting choices
-}
createQuestion : Seed -> List Category -> ( Seed, List Choice )
createQuestion seed cats =
    let
        myCats =
            case cats of
                [] ->
                    [ USCapitals ]

                _ ->
                    cats

        justMyCats : List Choice
        justMyCats =
            filter
                (\c ->
                    member c.category myCats
                )
                allChoices

        getRandomElement : Seed -> List a -> ( Seed, Maybe a )
        getRandomElement s list =
            let
                ( index, ss ) =
                    step (int 0 (length list - 1)) s
            in
            ( ss, drop index list |> head )

        ( s1, cc ) =
            getRandomElement seed justMyCats

        nonCenter =
            filter ((/=) (withDefault emptyChoice cc)) justMyCats

        ( left, right ) =
            partition (isLeftOf (withDefault emptyChoice cc)) nonCenter

        ( topLeft, bottomLeft ) =
            partition (isAbove (withDefault emptyChoice cc)) left

        ( topRight, bottomRight ) =
            partition (isAbove (withDefault emptyChoice cc)) right

        ( s2, ne ) =
            getRandomElement s1 topRight

        ( s3, nw ) =
            getRandomElement s2 topLeft

        ( s4, se ) =
            getRandomElement s3 bottomRight

        ( s5, sw ) =
            getRandomElement s4 bottomLeft

        choices =
            [ withDefault emptyChoice cc
            , withDefault emptyChoice ne
            , withDefault emptyChoice nw
            , withDefault emptyChoice se
            , withDefault emptyChoice sw
            ]
    in
    -- in case any choices are empty, run again
    -- in the case of USCapitals, the Southeast quadrant is very sparse
    -- so will often fail if an Eastern capital is at the center
    if List.any ((==) emptyChoice) choices then
        createQuestion
            (warning "Produced empty choice " cc s5)
            cats

    else
        ( s5, choices )


emptyChoice : Choice
emptyChoice =
    { category = EmptyCategory
    , detail = ""
    , measureX = 0.0
    , measureY = 0.0
    , name = ""
    , potIndex = -1
    }


{-| Default category selection, used in tests
-}
favoredCategory =
    USCapitals


{-| Get choice by index
-}
getChoiceNameAt : Int -> List Choice -> String
getChoiceNameAt i list =
    let
        prompts =
            [ "Northwestern most city"
            , "Northeastern most city"
            , "Southeastern most city"
            , "Southwestern most city"
            , "Centrally located city"
            ]

        choice =
            List.Extra.dropWhile (\c -> c.potIndex /= i) list |> head
    in
    case choice of
        Nothing ->
            withDefault "Unknown prompt" <| getAt i prompts

        Just c ->
            c.name


{-| Get all the names from all the choices of a question
-}
getChoiceNames : List Choice -> List String
getChoiceNames q =
    map (\choice -> choice.name) q


isAbove : Choice -> Choice -> Bool
isAbove center x =
    x.measureY > center.measureY


isLeftOf : Choice -> Choice -> Bool
isLeftOf center x =
    x.measureX < center.measureX


partition : (a -> Bool) -> List a -> ( List a, List a )
partition pred xs =
    ( filter pred xs, filter (pred >> not) xs )


{-| Send a message to the console, like Debug.log but doesn't printed value
-- does not have to match the return value.
-}
warning : String -> a -> b -> b
warning message badValue returnValue =
    let
        _ =
            Debug.log message badValue
    in
    returnValue


allChoices : List Choice
allChoices =
    [ { measureX = 54.367, measureY = 24.467, name = "Abu Dhabi", detail = "United Arab Emirates", potIndex = -1, category = WorldCapitals }
    , { measureX = 38.74, measureY = 9.03, name = "Addis Ababa", detail = "Ethiopia", potIndex = -1, category = WorldCapitals }
    , { measureX = -102.296, measureY = 21.876, name = "Aguascalientes", detail = "Aguascalientes", potIndex = -1, category = MXCapitals }
    , { measureX = -73.757, measureY = 42.653, name = "Albany", detail = "New York", potIndex = -1, category = USCapitals }
    , { measureX = 3.217, measureY = 36.767, name = "Algiers", detail = "Algeria", potIndex = -1, category = WorldCapitals }
    , { measureX = -169.875, measureY = -19.059, name = "Alofi", detail = "Niue", potIndex = -1, category = WorldCapitals }
    , { measureX = -76.501, measureY = 38.973, name = "Annapolis", detail = "Maryland", potIndex = -1, category = USCapitals }
    , { measureX = 47.517, measureY = -18.933, name = "Antananarivo", detail = "Madagascar", potIndex = -1, category = WorldCapitals }
    , { measureX = 38.933, measureY = 15.333, name = "Asmara", detail = "Eritrea", potIndex = -1, category = WorldCapitals }
    , { measureX = 23.717, measureY = 37.967, name = "Athens", detail = "Greece", potIndex = -1, category = WorldCapitals }
    , { measureX = -84.39, measureY = 33.755, name = "Atlanta", detail = "Georgia", potIndex = -1, category = USCapitals }
    , { measureX = -69.765, measureY = 44.324, name = "Augusta", detail = "Maine", potIndex = -1, category = USCapitals }
    , { measureX = -97.75, measureY = 30.25, name = "Austin", detail = "Texas", potIndex = -1, category = USCapitals }
    , { measureX = 49.882, measureY = 40.395, name = "Baku", detail = "Azerbaijan", potIndex = -1, category = WorldCapitals }
    , { measureX = -8, measureY = 12.65, name = "Bamako", detail = "Mali", potIndex = -1, category = WorldCapitals }
    , { measureX = 114.942, measureY = 4.89, name = "Bandar Seri Begawan", detail = "Brunei", potIndex = -1, category = WorldCapitals }
    , { measureX = 100.467, measureY = 13.75, name = "Bangkok", detail = "Thailand", potIndex = -1, category = WorldCapitals }
    , { measureX = 18.583, measureY = 4.367, name = "Bangui", detail = "Central African Republic", potIndex = -1, category = WorldCapitals }
    , { measureX = -16.578, measureY = 13.453, name = "Banjul", detail = "Gambia", potIndex = -1, category = WorldCapitals }
    , { measureX = -91.14, measureY = 30.45, name = "Baton Rouge", detail = "Louisiana", potIndex = -1, category = USCapitals }
    , { measureX = 35.513, measureY = 33.887, name = "Beirut", detail = "Lebanon", potIndex = -1, category = WorldCapitals }
    , { measureX = 20.467, measureY = 44.817, name = "Belgrade", detail = "Serbia", potIndex = -1, category = WorldCapitals }
    , { measureX = -100.779, measureY = 46.813, name = "Bismarck", detail = "North Dakota", potIndex = -1, category = USCapitals }
    , { measureX = -116.238, measureY = 43.614, name = "Boise", detail = "Idaho", potIndex = -1, category = USCapitals }
    , { measureX = -71.064, measureY = 42.358, name = "Boston", detail = "Massachusetts", potIndex = -1, category = USCapitals }
    , { measureX = -47.867, measureY = -15.799, name = "Brasília", detail = "Brazil", potIndex = -1, category = WorldCapitals }
    , { measureX = 17.11, measureY = 48.144, name = "Bratislava", detail = "Slovakia", potIndex = -1, category = WorldCapitals }
    , { measureX = 26.104, measureY = 44.433, name = "Bucharest", detail = "Romania", potIndex = -1, category = WorldCapitals }
    , { measureX = 19.05, measureY = 47.472, name = "Budapest", detail = "Hungary", potIndex = -1, category = WorldCapitals }
    , { measureX = -58.382, measureY = -34.603, name = "Buenos Aires", detail = "Argentina", potIndex = -1, category = WorldCapitals }
    , { measureX = 29.367, measureY = -3.383, name = "Bujumbura", detail = "Burundi", potIndex = -1, category = WorldCapitals }
    , { measureX = -90.531, measureY = 19.85, name = "Campeche", detail = "Campeche", potIndex = -1, category = MXCapitals }
    , { measureX = 149.124, measureY = -35.308, name = "Canberra", detail = "Australia", potIndex = -1, category = WorldCapitals }
    , { measureX = -66.917, measureY = 10.5, name = "Caracas", detail = "Venezuela", potIndex = -1, category = WorldCapitals }
    , { measureX = -119.754, measureY = 39.161, name = "Carson City", detail = "Nevada", potIndex = -1, category = USCapitals }
    , { measureX = -81.633, measureY = 38.347, name = "Charleston", detail = "West Virginia", potIndex = -1, category = USCapitals }
    , { measureX = -64.95, measureY = 18.35, name = "Charlotte Amalie", detail = "United States Virgin Islands", potIndex = -1, category = WorldCapitals }
    , { measureX = -88.305, measureY = 18.504, name = "Chetumal", detail = "Quintana Roo", potIndex = -1, category = MXCapitals }
    , { measureX = -104.802, measureY = 41.146, name = "Cheyenne", detail = "Wyoming", potIndex = -1, category = USCapitals }
    , { measureX = -106.089, measureY = 28.635, name = "Chihuahua", detail = "Chihuahua", potIndex = -1, category = MXCapitals }
    , { measureX = -99.5, measureY = 17.55, name = "Chilpancingo", detail = "Guerrero", potIndex = -1, category = MXCapitals }
    , { measureX = 4.35, measureY = 50.85, name = "City of Brussels", detail = "Belgium", potIndex = -1, category = WorldCapitals }
    , { measureX = -99.133, measureY = 23.733, name = "Ciudad Victoria", detail = "Tamaulipas", potIndex = -1, category = MXCapitals }
    , { measureX = -71.139, measureY = 21.459, name = "Cockburn Town", detail = "Turks and Caicos Islands", potIndex = -1, category = WorldCapitals }
    , { measureX = -103.725, measureY = 19.243, name = "Colima", detail = "Colima", potIndex = -1, category = MXCapitals }
    , { measureX = -81.035, measureY = 34.001, name = "Columbia", detail = "South Carolina", potIndex = -1, category = USCapitals }
    , { measureX = -82.983, measureY = 39.983, name = "Columbus", detail = "Ohio", potIndex = -1, category = USCapitals }
    , { measureX = -13.712, measureY = 9.509, name = "Conakry", detail = "Guinea", potIndex = -1, category = WorldCapitals }
    , { measureX = -71.538, measureY = 43.207, name = "Concord", detail = "New Hampshire", potIndex = -1, category = USCapitals }
    , { measureX = 12.568, measureY = 55.676, name = "Copenhagen", detail = "Denmark", potIndex = -1, category = WorldCapitals }
    , { measureX = -99.227, measureY = 18.918, name = "Cuernavaca", detail = "Morelos", potIndex = -1, category = MXCapitals }
    , { measureX = -107.383, measureY = 24.8, name = "Culiacán", detail = "Sinaloa", potIndex = -1, category = MXCapitals }
    , { measureX = -104.985, measureY = 39.739, name = "Denver", detail = "Colorado", potIndex = -1, category = USCapitals }
    , { measureX = -93.621, measureY = 41.591, name = "Des Moines", detail = "Iowa", potIndex = -1, category = USCapitals }
    , { measureX = 90.375, measureY = 23.7, name = "Dhaka", detail = "Bangladesh", potIndex = -1, category = WorldCapitals }
    , { measureX = 125.583, measureY = -8.55, name = "Dili", detail = "Timor-Leste", potIndex = -1, category = WorldCapitals }
    , { measureX = 43.145, measureY = 11.588, name = "Djibouti", detail = "Djibouti", potIndex = -1, category = WorldCapitals }
    , { measureX = -4.482, measureY = 54.145, name = "Douglas", detail = "Isle of Man", potIndex = -1, category = WorldCapitals }
    , { measureX = -75.527, measureY = 39.162, name = "Dover", detail = "Delaware", potIndex = -1, category = USCapitals }
    , { measureX = -6.26, measureY = 53.348, name = "Dublin", detail = "Ireland", potIndex = -1, category = WorldCapitals }
    , { measureX = -104.667, measureY = 24.017, name = "Durango", detail = "Durango", potIndex = -1, category = MXCapitals }
    , { measureX = 68.78, measureY = 38.537, name = "Dushanbe", detail = "Tajikistan", potIndex = -1, category = WorldCapitals }
    , { measureX = 105.678, measureY = -10.422, name = "Flying Fish Cove", detail = "Christmas Island", potIndex = -1, category = WorldCapitals }
    , { measureX = -84.863, measureY = 38.197, name = "Frankfort", detail = "Kentucky", potIndex = -1, category = USCapitals }
    , { measureX = -13.234, measureY = 8.484, name = "Freetown", detail = "Sierra Leone", potIndex = -1, category = WorldCapitals }
    , { measureX = 25.912, measureY = -24.658, name = "Gaborone", detail = "Botswana", potIndex = -1, category = WorldCapitals }
    , { measureX = -103.35, measureY = 20.667, name = "Guadalajara", detail = "Jalisco", potIndex = -1, category = MXCapitals }
    , { measureX = -101.257, measureY = 21.018, name = "Guanajuato", detail = "Guanajuato", potIndex = -1, category = MXCapitals }
    , { measureX = -64.782, measureY = 32.293, name = "Hamilton", detail = "Bermuda", potIndex = -1, category = WorldCapitals }
    , { measureX = 31.03, measureY = -17.864, name = "Harare", detail = "Zimbabwe", potIndex = -1, category = WorldCapitals }
    , { measureX = -76.876, measureY = 40.27, name = "Harrisburg", detail = "Pennsylvania", potIndex = -1, category = USCapitals }
    , { measureX = -72.674, measureY = 41.763, name = "Hartford", detail = "Connecticut", potIndex = -1, category = USCapitals }
    , { measureX = -82.383, measureY = 23.133, name = "Havana", detail = "Cuba", potIndex = -1, category = WorldCapitals }
    , { measureX = -112.027, measureY = 46.596, name = "Helena", detail = "Montana", potIndex = -1, category = USCapitals }
    , { measureX = -110.954, measureY = 29.099, name = "Hermosillo", detail = "Sonora", potIndex = -1, category = MXCapitals }
    , { measureX = -157.967, measureY = 21.467, name = "Honolulu County", detail = "Hawaii", potIndex = -1, category = USCapitals }
    , { measureX = -86.148, measureY = 39.791, name = "Indianapolis", detail = "Indiana", potIndex = -1, category = USCapitals }
    , { measureX = -90.185, measureY = 32.299, name = "Jackson", detail = "Mississippi", potIndex = -1, category = USCapitals }
    , { measureX = 106.8, measureY = -6.2, name = "Jakarta", detail = "Indonesia", potIndex = -1, category = WorldCapitals }
    , { measureX = -92.174, measureY = 38.577, name = "Jefferson City", detail = "Missouri", potIndex = -1, category = USCapitals }
    , { measureX = -134.417, measureY = 58.3, name = "Juneau", detail = "Alaska", potIndex = -1, category = USCapitals }
    , { measureX = 69.167, measureY = 34.533, name = "Kabul", detail = "Afghanistan", potIndex = -1, category = WorldCapitals }
    , { measureX = 32.581, measureY = 0.314, name = "Kampala", detail = "Uganda", potIndex = -1, category = WorldCapitals }
    , { measureX = 30.523, measureY = 50.45, name = "Kiev", detail = "Ukraine", potIndex = -1, category = WorldCapitals }
    , { measureX = 30.059, measureY = -1.944, name = "Kigali", detail = "Rwanda", potIndex = -1, category = WorldCapitals }
    , { measureX = 101.688, measureY = 3.136, name = "Kuala Lumpur", detail = "Malaysia", potIndex = -1, category = WorldCapitals }
    , { measureX = -110.311, measureY = 24.142, name = "La Paz", detail = "Baja California Sur", potIndex = -1, category = MXCapitals }
    , { measureX = -84.547, measureY = 42.734, name = "Lansing", detail = "Michigan", potIndex = -1, category = USCapitals }
    , { measureX = -77.028, measureY = -12.043, name = "Lima", detail = "Peru", potIndex = -1, category = WorldCapitals }
    , { measureX = -96.675, measureY = 40.81, name = "Lincoln", detail = "Nebraska", potIndex = -1, category = USCapitals }
    , { measureX = -9.139, measureY = 38.714, name = "Lisbon", detail = "Portugal", potIndex = -1, category = WorldCapitals }
    , { measureX = -92.331, measureY = 34.736, name = "Little Rock", detail = "Arkansas", potIndex = -1, category = USCapitals }
    , { measureX = -0.128, measureY = 51.507, name = "London", detail = "United Kingdom", potIndex = -1, category = WorldCapitals }
    , { measureX = -89.4, measureY = 43.067, name = "Madison", detail = "Wisconsin", potIndex = -1, category = USCapitals }
    , { measureX = -3.683, measureY = 40.4, name = "Madrid", detail = "Spain", potIndex = -1, category = WorldCapitals }
    , { measureX = 73.509, measureY = 4.175, name = "Malé", detail = "Maldives", potIndex = -1, category = WorldCapitals }
    , { measureX = 32.583, measureY = -25.967, name = "Maputo", detail = "Mozambique", potIndex = -1, category = WorldCapitals }
    , { measureX = 31.133, measureY = -26.317, name = "Mbabane", detail = "Swaziland", potIndex = -1, category = WorldCapitals }
    , { measureX = -89.62, measureY = 20.97, name = "Mérida", detail = "Yucatán", potIndex = -1, category = MXCapitals }
    , { measureX = -115.468, measureY = 32.663, name = "Mexicali", detail = "Baja California", potIndex = -1, category = MXCapitals }
    , { measureX = -99.133, measureY = 19.433, name = "Mexico City", detail = "Mexico", potIndex = -1, category = WorldCapitals }
    , { measureX = 27.567, measureY = 53.9, name = "Minsk", detail = "Belarus", potIndex = -1, category = WorldCapitals }
    , { measureX = -10.801, measureY = 6.313, name = "Monrovia", detail = "Liberia", potIndex = -1, category = WorldCapitals }
    , { measureX = -100.3, measureY = 25.667, name = "Monterrey", detail = "Nuevo León", potIndex = -1, category = MXCapitals }
    , { measureX = -56.182, measureY = -34.884, name = "Montevideo", detail = "Uruguay", potIndex = -1, category = WorldCapitals }
    , { measureX = -86.279, measureY = 32.362, name = "Montgomery", detail = "Alabama", potIndex = -1, category = USCapitals }
    , { measureX = -72.567, measureY = 44.25, name = "Montpelier", detail = "Vermont", potIndex = -1, category = USCapitals }
    , { measureX = -101.189, measureY = 19.768, name = "Morelia", detail = "Michoacán", potIndex = -1, category = MXCapitals }
    , { measureX = -86.783, measureY = 36.167, name = "Nashville", detail = "Tennessee", potIndex = -1, category = USCapitals }
    , { measureX = 77.209, measureY = 28.614, name = "New Delhi", detail = "India", potIndex = -1, category = WorldCapitals }
    , { measureX = 2.105, measureY = 13.521, name = "Niamey", detail = "Niger", potIndex = -1, category = WorldCapitals }
    , { measureX = 33.367, measureY = 35.167, name = "Nicosia", detail = "Cyprus", potIndex = -1, category = WorldCapitals }
    , { measureX = 166.458, measureY = -22.276, name = "Nouméa", detail = "New Caledonia", potIndex = -1, category = WorldCapitals }
    , { measureX = -51.739, measureY = 64.175, name = "Nuuk", detail = "Greenland", potIndex = -1, category = WorldCapitals }
    , { measureX = -96.75, measureY = 17.083, name = "Oaxaca", detail = "Oaxaca", potIndex = -1, category = MXCapitals }
    , { measureX = -97.535, measureY = 35.482, name = "Oklahoma City", detail = "Oklahoma", potIndex = -1, category = USCapitals }
    , { measureX = -122.893, measureY = 47.043, name = "Olympia", detail = "Washington", potIndex = -1, category = USCapitals }
    , { measureX = 10.75, measureY = 59.95, name = "Oslo", detail = "Norway", potIndex = -1, category = WorldCapitals }
    , { measureX = -1.535, measureY = 12.357, name = "Ouagadougou", detail = "Burkina Faso", potIndex = -1, category = WorldCapitals }
    , { measureX = -98.736, measureY = 20.122, name = "Pachuca", detail = "Hidalgo", potIndex = -1, category = MXCapitals }
    , { measureX = -170.701, measureY = -14.279, name = "Pago Pago", detail = "American Samoa", potIndex = -1, category = WorldCapitals }
    , { measureX = -55.167, measureY = 5.867, name = "Paramaribo", detail = "Suriname", potIndex = -1, category = WorldCapitals }
    , { measureX = 2.351, measureY = 48.857, name = "Paris", detail = "France", potIndex = -1, category = WorldCapitals }
    , { measureX = 104.917, measureY = 11.55, name = "Phnom Penh", detail = "Cambodia", potIndex = -1, category = WorldCapitals }
    , { measureX = -112.067, measureY = 33.45, name = "Phoenix", detail = "Arizona", potIndex = -1, category = USCapitals }
    , { measureX = -100.336, measureY = 44.368, name = "Pierre", detail = "South Dakota", potIndex = -1, category = USCapitals }
    , { measureX = 57.504, measureY = -20.164, name = "Port Louis", detail = "Mauritius", potIndex = -1, category = WorldCapitals }
    , { measureX = 2.605, measureY = 6.497, name = "Porto-Novo", detail = "Benin", potIndex = -1, category = WorldCapitals }
    , { measureX = 14.417, measureY = 50.083, name = "Prague", detail = "Czech Republic", potIndex = -1, category = WorldCapitals }
    , { measureX = -71.422, measureY = 41.824, name = "Providence", detail = "Rhode Island", potIndex = -1, category = USCapitals }
    , { measureX = -98.218, measureY = 19.051, name = "Puebla", detail = "Puebla", potIndex = -1, category = MXCapitals }
    , { measureX = -100.393, measureY = 20.588, name = "Querétaro", detail = "Querétaro", potIndex = -1, category = MXCapitals }
    , { measureX = -78.583, measureY = -0.25, name = "Quito", detail = "Ecuador", potIndex = -1, category = WorldCapitals }
    , { measureX = -6.842, measureY = 34.021, name = "Rabat", detail = "Morocco", potIndex = -1, category = WorldCapitals }
    , { measureX = -78.645, measureY = 35.819, name = "Raleigh", detail = "North Carolina", potIndex = -1, category = USCapitals }
    , { measureX = -21.933, measureY = 64.133, name = "Reykjavik", detail = "Iceland", potIndex = -1, category = WorldCapitals }
    , { measureX = -77.433, measureY = 37.541, name = "Richmond", detail = "Virginia", potIndex = -1, category = USCapitals }
    , { measureX = 46.717, measureY = 24.633, name = "Riyadh", detail = "Saudi Arabia", potIndex = -1, category = WorldCapitals }
    , { measureX = -64.623, measureY = 18.431, name = "Road Town", detail = "British Virgin Islands", potIndex = -1, category = WorldCapitals }
    , { measureX = 12.5, measureY = 41.9, name = "Rome", detail = "Italy", potIndex = -1, category = WorldCapitals }
    , { measureX = -121.469, measureY = 38.556, name = "Sacramento", detail = "California", potIndex = -1, category = USCapitals }
    , { measureX = -93.085, measureY = 44.944, name = "Saint Paul", detail = "Minnesota", potIndex = -1, category = USCapitals }
    , { measureX = -2.537, measureY = 49.456, name = "Saint Peter Port", detail = "Guernsey", potIndex = -1, category = WorldCapitals }
    , { measureX = -56.178, measureY = 46.778, name = "Saint-Pierre", detail = "Saint Pierre and Miquelon", potIndex = -1, category = WorldCapitals }
    , { measureX = 145.75, measureY = 15.183, name = "Saipan", detail = "Northern Mariana Islands", potIndex = -1, category = WorldCapitals }
    , { measureX = -123.029, measureY = 44.931, name = "Salem", detail = "Oregon", potIndex = -1, category = USCapitals }
    , { measureX = -111.883, measureY = 40.75, name = "Salt Lake City", detail = "Utah", potIndex = -1, category = USCapitals }
    , { measureX = -101, measureY = 25.417, name = "Saltillo", detail = "Coahuila", potIndex = -1, category = MXCapitals }
    , { measureX = -100.843, measureY = 22.151, name = "San Luis Potosí", detail = "San Luis Potosí", potIndex = -1, category = MXCapitals }
    , { measureX = 12.447, measureY = 43.935, name = "San Marino", detail = "San Marino", potIndex = -1, category = WorldCapitals }
    , { measureX = -105.964, measureY = 35.667, name = "Santa Fe", detail = "New Mexico", potIndex = -1, category = USCapitals }
    , { measureX = 21.433, measureY = 42.001, name = "Skopje", detail = "Republic of Macedonia", potIndex = -1, category = WorldCapitals }
    , { measureX = 23.333, measureY = 42.7, name = "Sofia", detail = "Bulgaria", potIndex = -1, category = WorldCapitals }
    , { measureX = -89.65, measureY = 39.783, name = "Springfield", detail = "Illinois", potIndex = -1, category = USCapitals }
    , { measureX = 79.888, measureY = 6.911, name = "Sri Jayawardenapura Kotte", detail = "Sri Lanka", potIndex = -1, category = WorldCapitals }
    , { measureX = -84.253, measureY = 30.455, name = "Tallahassee", detail = "Florida", potIndex = -1, category = USCapitals }
    , { measureX = 24.745, measureY = 59.437, name = "Tallinn", detail = "Estonia", potIndex = -1, category = WorldCapitals }
    , { measureX = 69.217, measureY = 41.267, name = "Tashkent", detail = "Uzbekistan", potIndex = -1, category = WorldCapitals }
    , { measureX = 44.783, measureY = 41.717, name = "Tbilisi", detail = "Georgia", potIndex = -1, category = WorldCapitals }
    , { measureX = -104.893, measureY = 21.508, name = "Tepic", detail = "Nayarit", potIndex = -1, category = MXCapitals }
    , { measureX = -63.052, measureY = 18.221, name = "The Valley", detail = "Anguilla", potIndex = -1, category = WorldCapitals }
    , { measureX = 89.642, measureY = 27.467, name = "Thimphu", detail = "Bhutan", potIndex = -1, category = WorldCapitals }
    , { measureX = -98.233, measureY = 19.3, name = "Tlaxcala", detail = "Tlaxcala", potIndex = -1, category = MXCapitals }
    , { measureX = 139.692, measureY = 35.69, name = "Tokyo", detail = "Japan", potIndex = -1, category = WorldCapitals }
    , { measureX = -99.657, measureY = 19.293, name = "Toluca", detail = "Mexico State", potIndex = -1, category = MXCapitals }
    , { measureX = -95.689, measureY = 39.056, name = "Topeka", detail = "Kansas", potIndex = -1, category = USCapitals }
    , { measureX = -74.764, measureY = 40.224, name = "Trenton", detail = "New Jersey", potIndex = -1, category = USCapitals }
    , { measureX = 13.186, measureY = 32.902, name = "Tripoli", detail = "Libya", potIndex = -1, category = WorldCapitals }
    , { measureX = -93.117, measureY = 16.753, name = "Tuxtla Gutiérrez", detail = "Chiapas", potIndex = -1, category = MXCapitals }
    , { measureX = 106.92, measureY = 47.92, name = "Ulan Bator", detail = "Mongolia", potIndex = -1, category = WorldCapitals }
    , { measureX = 14.513, measureY = 35.898, name = "Valletta", detail = "Malta", potIndex = -1, category = WorldCapitals }
    , { measureX = 55.45, measureY = -4.617, name = "Victoria", detail = "Seychelles", potIndex = -1, category = WorldCapitals }
    , { measureX = 16.373, measureY = 48.208, name = "Vienna", detail = "Austria", potIndex = -1, category = WorldCapitals }
    , { measureX = 102.6, measureY = 17.967, name = "Vientiane", detail = "Laos", potIndex = -1, category = WorldCapitals }
    , { measureX = -92.933, measureY = 17.983, name = "Villahermosa", detail = "Tabasco", potIndex = -1, category = MXCapitals }
    , { measureX = 25.283, measureY = 54.683, name = "Vilnius", detail = "Lithuania", potIndex = -1, category = WorldCapitals }
    , { measureX = -96.928, measureY = 19.54, name = "Xalapa", detail = "Veracruz", potIndex = -1, category = MXCapitals }
    , { measureX = 11.517, measureY = 3.867, name = "Yaoundé", detail = "Cameroon", potIndex = -1, category = WorldCapitals }
    , { measureX = 44.517, measureY = 40.183, name = "Yerevan", detail = "Armenia", potIndex = -1, category = WorldCapitals }
    , { measureX = -102.55, measureY = 22.767, name = "Zacatecas", detail = "Zacatecas", potIndex = -1, category = MXCapitals }
    , { measureX = 15.983, measureY = 45.817, name = "Zagreb", detail = "Croatia", potIndex = -1, category = WorldCapitals }
    ]
