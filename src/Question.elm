module Question exposing (Question, getQuestion)

import Random exposing (Seed, int, step)
import Tuple exposing (second)


type alias Choice =
    { measureX : Float
    , measureY : Float
    , name : String
    }


type alias Question =
    { choices : List Choice
    }


getQuestion : Seed -> ( Seed, Question )
getQuestion seed =
    ( step (int 1 10) seed |> second, { choices = allChoices } )


allChoices : List Choice
allChoices =
    [ { measureX = 1.0, measureY = 1.0, name = "north west" }
    , { measureX = 1.0, measureY = 2.0, name = "south west" }
    , { measureX = 1.5, measureY = 1.5, name = "center" }
    , { measureX = 2.0, measureY = 2.0, name = "north east" }
    , { measureX = 2.0, measureY = 1.0, name = "south east" }
    ]
