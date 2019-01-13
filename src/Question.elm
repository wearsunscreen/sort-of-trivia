module Question exposing (Category(..), Question, createQuestion)

import List exposing (drop, filter, head, length, member, reverse, sortBy, take)
import Maybe exposing (withDefault)
import Random exposing (Seed, int, step)
import Tuple exposing (second)


type Category
    = USCapitals
    | MXCapitals
    | WorldCapitals
    | EmptyCategory


type alias Choice =
    { measureX : Float
    , measureY : Float
    , city : String
    , state : String
    , category : Category
    }


type alias Question =
    { centerChoice : Choice
    , neChoice : Choice
    , nwChoice : Choice
    , seChoice : Choice
    , swChoice : Choice
    }


{-| create a multiple choice question by randomly selecting choices
-}
createQuestion : Seed -> List Category -> ( Seed, Question )
createQuestion seed cats =
    let
        cats_ =
            case cats of
                [] ->
                    [ USCapitals ]

                _ ->
                    cats

        goodChoices =
            filter (\c -> member c.category cats_) allChoices

        cc =
            withDefault emptyChoice <| head goodChoices

        ne =
            withDefault emptyChoice <| head <| drop 1 goodChoices

        nw =
            withDefault emptyChoice <| head <| drop 2 goodChoices

        se =
            withDefault emptyChoice <| head <| drop 3 goodChoices

        sw =
            withDefault emptyChoice <| head <| drop 4 goodChoices
    in
    ( step (int 1 10) seed |> second
    , { centerChoice = cc
      , neChoice = ne
      , nwChoice = nw
      , seChoice = se
      , swChoice = sw
      }
    )


centerChoices : List Choice -> List Choice
centerChoices choices =
    let
        margin =
            length choices // 10

        noLeft =
            drop margin <| sortBy .measureX allChoices
    in
    drop margin <| reverse noLeft


emptyChoice : Choice
emptyChoice =
    { measureX = 0.0
    , measureY = 0.0
    , city = "empty"
    , state = "empty"
    , category = EmptyCategory
    }


allChoices : List Choice
allChoices =
    [ { measureX = 54.367, measureY = 24.467, city = "Abu Dhabi", state = "United Arab Emirates", category = WorldCapitals }
    , { measureX = 38.74, measureY = 9.03, city = "Addis Ababa", state = "Ethiopia", category = WorldCapitals }
    , { measureX = -102.296, measureY = 21.876, city = "Aguascalientes", state = "Aguascalientes", category = MXCapitals }
    , { measureX = -73.757, measureY = 42.653, city = "Albany", state = "New York", category = USCapitals }
    , { measureX = 3.217, measureY = 36.767, city = "Algiers", state = "Algeria", category = WorldCapitals }
    , { measureX = -169.875, measureY = -19.059, city = "Alofi", state = "Niue", category = WorldCapitals }
    , { measureX = -76.501, measureY = 38.973, city = "Annapolis", state = "Maryland", category = USCapitals }
    , { measureX = 47.517, measureY = -18.933, city = "Antananarivo", state = "Madagascar", category = WorldCapitals }
    , { measureX = 38.933, measureY = 15.333, city = "Asmara", state = "Eritrea", category = WorldCapitals }
    , { measureX = 23.717, measureY = 37.967, city = "Athens", state = "Greece", category = WorldCapitals }
    , { measureX = -84.39, measureY = 33.755, city = "Atlanta", state = "Georgia", category = USCapitals }
    , { measureX = -69.765, measureY = 44.324, city = "Augusta", state = "Maine", category = USCapitals }
    , { measureX = -97.75, measureY = 30.25, city = "Austin", state = "Texas", category = USCapitals }
    , { measureX = 49.882, measureY = 40.395, city = "Baku", state = "Azerbaijan", category = WorldCapitals }
    , { measureX = -8, measureY = 12.65, city = "Bamako", state = "Mali", category = WorldCapitals }
    , { measureX = 114.942, measureY = 4.89, city = "Bandar Seri Begawan", state = "Brunei", category = WorldCapitals }
    , { measureX = 100.467, measureY = 13.75, city = "Bangkok", state = "Thailand", category = WorldCapitals }
    , { measureX = 18.583, measureY = 4.367, city = "Bangui", state = "Central African Republic", category = WorldCapitals }
    , { measureX = -16.578, measureY = 13.453, city = "Banjul", state = "Gambia", category = WorldCapitals }
    , { measureX = -91.14, measureY = 30.45, city = "Baton Rouge", state = "Louisiana", category = USCapitals }
    , { measureX = 35.513, measureY = 33.887, city = "Beirut", state = "Lebanon", category = WorldCapitals }
    , { measureX = 20.467, measureY = 44.817, city = "Belgrade", state = "Serbia", category = WorldCapitals }
    , { measureX = -100.779, measureY = 46.813, city = "Bismarck", state = "North Dakota", category = USCapitals }
    , { measureX = -116.238, measureY = 43.614, city = "Boise", state = "Idaho", category = USCapitals }
    , { measureX = -71.064, measureY = 42.358, city = "Boston", state = "Massachusetts", category = USCapitals }
    , { measureX = -47.867, measureY = -15.799, city = "Brasília", state = "Brazil", category = WorldCapitals }
    , { measureX = 17.11, measureY = 48.144, city = "Bratislava", state = "Slovakia", category = WorldCapitals }
    , { measureX = 26.104, measureY = 44.433, city = "Bucharest", state = "Romania", category = WorldCapitals }
    , { measureX = 19.05, measureY = 47.472, city = "Budapest", state = "Hungary", category = WorldCapitals }
    , { measureX = -58.382, measureY = -34.603, city = "Buenos Aires", state = "Argentina", category = WorldCapitals }
    , { measureX = 29.367, measureY = -3.383, city = "Bujumbura", state = "Burundi", category = WorldCapitals }
    , { measureX = -90.531, measureY = 19.85, city = "Campeche", state = "Campeche", category = MXCapitals }
    , { measureX = 149.124, measureY = -35.308, city = "Canberra", state = "Australia", category = WorldCapitals }
    , { measureX = -66.917, measureY = 10.5, city = "Caracas", state = "Venezuela", category = WorldCapitals }
    , { measureX = -119.754, measureY = 39.161, city = "Carson City", state = "Nevada", category = USCapitals }
    , { measureX = -81.633, measureY = 38.347, city = "Charleston", state = "West Virginia", category = USCapitals }
    , { measureX = -64.95, measureY = 18.35, city = "Charlotte Amalie", state = "United States Virgin Islands", category = WorldCapitals }
    , { measureX = -88.305, measureY = 18.504, city = "Chetumal", state = "Quintana Roo", category = MXCapitals }
    , { measureX = -104.802, measureY = 41.146, city = "Cheyenne", state = "Wyoming", category = USCapitals }
    , { measureX = -106.089, measureY = 28.635, city = "Chihuahua", state = "Chihuahua", category = MXCapitals }
    , { measureX = -99.5, measureY = 17.55, city = "Chilpancingo", state = "Guerrero", category = MXCapitals }
    , { measureX = 4.35, measureY = 50.85, city = "City of Brussels", state = "Belgium", category = WorldCapitals }
    , { measureX = -99.133, measureY = 23.733, city = "Ciudad Victoria", state = "Tamaulipas", category = MXCapitals }
    , { measureX = -71.139, measureY = 21.459, city = "Cockburn Town", state = "Turks and Caicos Islands", category = WorldCapitals }
    , { measureX = -103.725, measureY = 19.243, city = "Colima", state = "Colima", category = MXCapitals }
    , { measureX = -81.035, measureY = 34.001, city = "Columbia", state = "South Carolina", category = USCapitals }
    , { measureX = -82.983, measureY = 39.983, city = "Columbus", state = "Ohio", category = USCapitals }
    , { measureX = -13.712, measureY = 9.509, city = "Conakry", state = "Guinea", category = WorldCapitals }
    , { measureX = -71.538, measureY = 43.207, city = "Concord", state = "New Hampshire", category = USCapitals }
    , { measureX = 12.568, measureY = 55.676, city = "Copenhagen", state = "Denmark", category = WorldCapitals }
    , { measureX = -99.227, measureY = 18.918, city = "Cuernavaca", state = "Morelos", category = MXCapitals }
    , { measureX = -107.383, measureY = 24.8, city = "Culiacán", state = "Sinaloa", category = MXCapitals }
    , { measureX = -104.985, measureY = 39.739, city = "Denver", state = "Colorado", category = USCapitals }
    , { measureX = -93.621, measureY = 41.591, city = "Des Moines", state = "Iowa", category = USCapitals }
    , { measureX = 90.375, measureY = 23.7, city = "Dhaka", state = "Bangladesh", category = WorldCapitals }
    , { measureX = 125.583, measureY = -8.55, city = "Dili", state = "Timor-Leste", category = WorldCapitals }
    , { measureX = 43.145, measureY = 11.588, city = "Djibouti", state = "Djibouti", category = WorldCapitals }
    , { measureX = -4.482, measureY = 54.145, city = "Douglas", state = "Isle of Man", category = WorldCapitals }
    , { measureX = -75.527, measureY = 39.162, city = "Dover", state = "Delaware", category = USCapitals }
    , { measureX = -6.26, measureY = 53.348, city = "Dublin", state = "Ireland", category = WorldCapitals }
    , { measureX = -104.667, measureY = 24.017, city = "Durango", state = "Durango", category = MXCapitals }
    , { measureX = 68.78, measureY = 38.537, city = "Dushanbe", state = "Tajikistan", category = WorldCapitals }
    , { measureX = 105.678, measureY = -10.422, city = "Flying Fish Cove", state = "Christmas Island", category = WorldCapitals }
    , { measureX = -84.863, measureY = 38.197, city = "Frankfort", state = "Kentucky", category = USCapitals }
    , { measureX = -13.234, measureY = 8.484, city = "Freetown", state = "Sierra Leone", category = WorldCapitals }
    , { measureX = 25.912, measureY = -24.658, city = "Gaborone", state = "Botswana", category = WorldCapitals }
    , { measureX = -103.35, measureY = 20.667, city = "Guadalajara", state = "Jalisco", category = MXCapitals }
    , { measureX = -101.257, measureY = 21.018, city = "Guanajuato", state = "Guanajuato", category = MXCapitals }
    , { measureX = -64.782, measureY = 32.293, city = "Hamilton", state = "Bermuda", category = WorldCapitals }
    , { measureX = 31.03, measureY = -17.864, city = "Harare", state = "Zimbabwe", category = WorldCapitals }
    , { measureX = -76.876, measureY = 40.27, city = "Harrisburg", state = "Pennsylvania", category = USCapitals }
    , { measureX = -72.674, measureY = 41.763, city = "Hartford", state = "Connecticut", category = USCapitals }
    , { measureX = -82.383, measureY = 23.133, city = "Havana", state = "Cuba", category = WorldCapitals }
    , { measureX = -112.027, measureY = 46.596, city = "Helena", state = "Montana", category = USCapitals }
    , { measureX = -110.954, measureY = 29.099, city = "Hermosillo", state = "Sonora", category = MXCapitals }
    , { measureX = -157.967, measureY = 21.467, city = "Honolulu County", state = "Hawaii", category = USCapitals }
    , { measureX = -86.148, measureY = 39.791, city = "Indianapolis", state = "Indiana", category = USCapitals }
    , { measureX = -90.185, measureY = 32.299, city = "Jackson", state = "Mississippi", category = USCapitals }
    , { measureX = 106.8, measureY = -6.2, city = "Jakarta", state = "Indonesia", category = WorldCapitals }
    , { measureX = -92.174, measureY = 38.577, city = "Jefferson City", state = "Missouri", category = USCapitals }
    , { measureX = -134.417, measureY = 58.3, city = "Juneau", state = "Alaska", category = USCapitals }
    , { measureX = 69.167, measureY = 34.533, city = "Kabul", state = "Afghanistan", category = WorldCapitals }
    , { measureX = 32.581, measureY = 0.314, city = "Kampala", state = "Uganda", category = WorldCapitals }
    , { measureX = 30.523, measureY = 50.45, city = "Kiev", state = "Ukraine", category = WorldCapitals }
    , { measureX = 30.059, measureY = -1.944, city = "Kigali", state = "Rwanda", category = WorldCapitals }
    , { measureX = 101.688, measureY = 3.136, city = "Kuala Lumpur", state = "Malaysia", category = WorldCapitals }
    , { measureX = -110.311, measureY = 24.142, city = "La Paz", state = "Baja California Sur", category = MXCapitals }
    , { measureX = -84.547, measureY = 42.734, city = "Lansing", state = "Michigan", category = USCapitals }
    , { measureX = -77.028, measureY = -12.043, city = "Lima", state = "Peru", category = WorldCapitals }
    , { measureX = -96.675, measureY = 40.81, city = "Lincoln", state = "Nebraska", category = USCapitals }
    , { measureX = -9.139, measureY = 38.714, city = "Lisbon", state = "Portugal", category = WorldCapitals }
    , { measureX = -92.331, measureY = 34.736, city = "Little Rock", state = "Arkansas", category = USCapitals }
    , { measureX = -0.128, measureY = 51.507, city = "London", state = "United Kingdom", category = WorldCapitals }
    , { measureX = -89.4, measureY = 43.067, city = "Madison", state = "Wisconsin", category = USCapitals }
    , { measureX = -3.683, measureY = 40.4, city = "Madrid", state = "Spain", category = WorldCapitals }
    , { measureX = 73.509, measureY = 4.175, city = "Malé", state = "Maldives", category = WorldCapitals }
    , { measureX = 32.583, measureY = -25.967, city = "Maputo", state = "Mozambique", category = WorldCapitals }
    , { measureX = 31.133, measureY = -26.317, city = "Mbabane", state = "Swaziland", category = WorldCapitals }
    , { measureX = -89.62, measureY = 20.97, city = "Mérida", state = "Yucatán", category = MXCapitals }
    , { measureX = -115.468, measureY = 32.663, city = "Mexicali", state = "Baja California", category = MXCapitals }
    , { measureX = -99.133, measureY = 19.433, city = "Mexico City", state = "Mexico", category = WorldCapitals }
    , { measureX = 27.567, measureY = 53.9, city = "Minsk", state = "Belarus", category = WorldCapitals }
    , { measureX = -10.801, measureY = 6.313, city = "Monrovia", state = "Liberia", category = WorldCapitals }
    , { measureX = -100.3, measureY = 25.667, city = "Monterrey", state = "Nuevo León", category = MXCapitals }
    , { measureX = -56.182, measureY = -34.884, city = "Montevideo", state = "Uruguay", category = WorldCapitals }
    , { measureX = -86.279, measureY = 32.362, city = "Montgomery", state = "Alabama", category = USCapitals }
    , { measureX = -72.567, measureY = 44.25, city = "Montpelier", state = "Vermont", category = USCapitals }
    , { measureX = -101.189, measureY = 19.768, city = "Morelia", state = "Michoacán", category = MXCapitals }
    , { measureX = -86.783, measureY = 36.167, city = "Nashville", state = "Tennessee", category = USCapitals }
    , { measureX = 77.209, measureY = 28.614, city = "New Delhi", state = "India", category = WorldCapitals }
    , { measureX = 2.105, measureY = 13.521, city = "Niamey", state = "Niger", category = WorldCapitals }
    , { measureX = 33.367, measureY = 35.167, city = "Nicosia", state = "Cyprus", category = WorldCapitals }
    , { measureX = 166.458, measureY = -22.276, city = "Nouméa", state = "New Caledonia", category = WorldCapitals }
    , { measureX = -51.739, measureY = 64.175, city = "Nuuk", state = "Greenland", category = WorldCapitals }
    , { measureX = -96.75, measureY = 17.083, city = "Oaxaca", state = "Oaxaca", category = MXCapitals }
    , { measureX = -97.535, measureY = 35.482, city = "Oklahoma City", state = "Oklahoma", category = USCapitals }
    , { measureX = -122.893, measureY = 47.043, city = "Olympia", state = "Washington", category = USCapitals }
    , { measureX = 10.75, measureY = 59.95, city = "Oslo", state = "Norway", category = WorldCapitals }
    , { measureX = -1.535, measureY = 12.357, city = "Ouagadougou", state = "Burkina Faso", category = WorldCapitals }
    , { measureX = -98.736, measureY = 20.122, city = "Pachuca", state = "Hidalgo", category = MXCapitals }
    , { measureX = -170.701, measureY = -14.279, city = "Pago Pago", state = "American Samoa", category = WorldCapitals }
    , { measureX = -55.167, measureY = 5.867, city = "Paramaribo", state = "Suriname", category = WorldCapitals }
    , { measureX = 2.351, measureY = 48.857, city = "Paris", state = "France", category = WorldCapitals }
    , { measureX = 104.917, measureY = 11.55, city = "Phnom Penh", state = "Cambodia", category = WorldCapitals }
    , { measureX = -112.067, measureY = 33.45, city = "Phoenix", state = "Arizona", category = USCapitals }
    , { measureX = -100.336, measureY = 44.368, city = "Pierre", state = "South Dakota", category = USCapitals }
    , { measureX = 57.504, measureY = -20.164, city = "Port Louis", state = "Mauritius", category = WorldCapitals }
    , { measureX = 2.605, measureY = 6.497, city = "Porto-Novo", state = "Benin", category = WorldCapitals }
    , { measureX = 14.417, measureY = 50.083, city = "Prague", state = "Czech Republic", category = WorldCapitals }
    , { measureX = -71.422, measureY = 41.824, city = "Providence", state = "Rhode Island", category = USCapitals }
    , { measureX = -98.218, measureY = 19.051, city = "Puebla", state = "Puebla", category = MXCapitals }
    , { measureX = -100.393, measureY = 20.588, city = "Querétaro", state = "Querétaro", category = MXCapitals }
    , { measureX = -78.583, measureY = -0.25, city = "Quito", state = "Ecuador", category = WorldCapitals }
    , { measureX = -6.842, measureY = 34.021, city = "Rabat", state = "Morocco", category = WorldCapitals }
    , { measureX = -78.645, measureY = 35.819, city = "Raleigh", state = "North Carolina", category = USCapitals }
    , { measureX = -21.933, measureY = 64.133, city = "Reykjavik", state = "Iceland", category = WorldCapitals }
    , { measureX = -77.433, measureY = 37.541, city = "Richmond", state = "Virginia", category = USCapitals }
    , { measureX = 46.717, measureY = 24.633, city = "Riyadh", state = "Saudi Arabia", category = WorldCapitals }
    , { measureX = -64.623, measureY = 18.431, city = "Road Town", state = "British Virgin Islands", category = WorldCapitals }
    , { measureX = 12.5, measureY = 41.9, city = "Rome", state = "Italy", category = WorldCapitals }
    , { measureX = -121.469, measureY = 38.556, city = "Sacramento", state = "California", category = USCapitals }
    , { measureX = -93.085, measureY = 44.944, city = "Saint Paul", state = "Minnesota", category = USCapitals }
    , { measureX = -2.537, measureY = 49.456, city = "Saint Peter Port", state = "Guernsey", category = WorldCapitals }
    , { measureX = -56.178, measureY = 46.778, city = "Saint-Pierre", state = "Saint Pierre and Miquelon", category = WorldCapitals }
    , { measureX = 145.75, measureY = 15.183, city = "Saipan", state = "Northern Mariana Islands", category = WorldCapitals }
    , { measureX = -123.029, measureY = 44.931, city = "Salem", state = "Oregon", category = USCapitals }
    , { measureX = -111.883, measureY = 40.75, city = "Salt Lake City", state = "Utah", category = USCapitals }
    , { measureX = -101, measureY = 25.417, city = "Saltillo", state = "Coahuila", category = MXCapitals }
    , { measureX = -100.843, measureY = 22.151, city = "San Luis Potosí", state = "San Luis Potosí", category = MXCapitals }
    , { measureX = 12.447, measureY = 43.935, city = "San Marino", state = "San Marino", category = WorldCapitals }
    , { measureX = -105.964, measureY = 35.667, city = "Santa Fe", state = "New Mexico", category = USCapitals }
    , { measureX = 21.433, measureY = 42.001, city = "Skopje", state = "Republic of Macedonia", category = WorldCapitals }
    , { measureX = 23.333, measureY = 42.7, city = "Sofia", state = "Bulgaria", category = WorldCapitals }
    , { measureX = -89.65, measureY = 39.783, city = "Springfield", state = "Illinois", category = USCapitals }
    , { measureX = 79.888, measureY = 6.911, city = "Sri Jayawardenapura Kotte", state = "Sri Lanka", category = WorldCapitals }
    , { measureX = -84.253, measureY = 30.455, city = "Tallahassee", state = "Florida", category = USCapitals }
    , { measureX = 24.745, measureY = 59.437, city = "Tallinn", state = "Estonia", category = WorldCapitals }
    , { measureX = 69.217, measureY = 41.267, city = "Tashkent", state = "Uzbekistan", category = WorldCapitals }
    , { measureX = 44.783, measureY = 41.717, city = "Tbilisi", state = "Georgia", category = WorldCapitals }
    , { measureX = -104.893, measureY = 21.508, city = "Tepic", state = "Nayarit", category = MXCapitals }
    , { measureX = -63.052, measureY = 18.221, city = "The Valley", state = "Anguilla", category = WorldCapitals }
    , { measureX = 89.642, measureY = 27.467, city = "Thimphu", state = "Bhutan", category = WorldCapitals }
    , { measureX = -98.233, measureY = 19.3, city = "Tlaxcala", state = "Tlaxcala", category = MXCapitals }
    , { measureX = 139.692, measureY = 35.69, city = "Tokyo", state = "Japan", category = WorldCapitals }
    , { measureX = -99.657, measureY = 19.293, city = "Toluca", state = "Mexico State", category = MXCapitals }
    , { measureX = -95.689, measureY = 39.056, city = "Topeka", state = "Kansas", category = USCapitals }
    , { measureX = -74.764, measureY = 40.224, city = "Trenton", state = "New Jersey", category = USCapitals }
    , { measureX = 13.186, measureY = 32.902, city = "Tripoli", state = "Libya", category = WorldCapitals }
    , { measureX = -93.117, measureY = 16.753, city = "Tuxtla Gutiérrez", state = "Chiapas", category = MXCapitals }
    , { measureX = 106.92, measureY = 47.92, city = "Ulan Bator", state = "Mongolia", category = WorldCapitals }
    , { measureX = 14.513, measureY = 35.898, city = "Valletta", state = "Malta", category = WorldCapitals }
    , { measureX = 55.45, measureY = -4.617, city = "Victoria", state = "Seychelles", category = WorldCapitals }
    , { measureX = 16.373, measureY = 48.208, city = "Vienna", state = "Austria", category = WorldCapitals }
    , { measureX = 102.6, measureY = 17.967, city = "Vientiane", state = "Laos", category = WorldCapitals }
    , { measureX = -92.933, measureY = 17.983, city = "Villahermosa", state = "Tabasco", category = MXCapitals }
    , { measureX = 25.283, measureY = 54.683, city = "Vilnius", state = "Lithuania", category = WorldCapitals }
    , { measureX = -96.928, measureY = 19.54, city = "Xalapa", state = "Veracruz", category = MXCapitals }
    , { measureX = 11.517, measureY = 3.867, city = "Yaoundé", state = "Cameroon", category = WorldCapitals }
    , { measureX = 44.517, measureY = 40.183, city = "Yerevan", state = "Armenia", category = WorldCapitals }
    , { measureX = -102.55, measureY = 22.767, city = "Zacatecas", state = "Zacatecas", category = MXCapitals }
    , { measureX = 15.983, measureY = 45.817, city = "Zagreb", state = "Croatia", category = WorldCapitals }
    ]
